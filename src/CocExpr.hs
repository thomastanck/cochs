{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CocExpr where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Foldable
import Data.Functor (void)
import Data.List (elemIndex)
import Data.Maybe(fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.String (fromString)

import CocSyntax
import Utils

type CocType = CocExpr
data CocExpr =
    -- Type of types
    -- e.g. CocForall (CocUnused) (CocVariable "A") (CocVariable "A")
    -- has type CocProp
    CocProp
    -- Type of CocProp
    | CocType
    -- Denotes a variable
    | CocVariable { index :: Int, label :: String }
    -- Denotes a hole
    | CocHole { label :: String }
    -- Denotes an application
    | CocApply { function :: CocExpr, argument :: CocExpr }
    -- Creates a function abstraction
    | CocLambda { label :: String, inType :: CocType,  body :: CocExpr }
    -- Creates a type abstraction
    | CocForall { label :: String, inType :: CocType,  body :: CocExpr }

instance Eq CocExpr where
    (==) CocProp CocProp =
        True
    (==) CocType CocType =
        True
    (==) (CocVariable a _) (CocVariable b _) =
        a == b
    (==) (CocApply a b) (CocApply c d) =
        a == c && b == d
    (==) (CocLambda _ a b) (CocLambda _ c d) =
        a == c && b == d
    (==) (CocForall _ a b) (CocForall _ c d) =
        a == c && b == d
    (==) _ _ =
        False

isArrowExpr (CocForall "_" _ _) = True
isArrowExpr _ = False


instance Show CocExpr where
    showsPrec prec x = let (_,(str,nl,l)) = evalIndent (showHelper x) in appEndo str
        where showMultiline :: [IndentData] -> IndentM ()
              showMultiline [a] = tell a
              showMultiline (a:as) = showMultiline as *> indentedNewLine *> tell a

              -- TODO: We always try both prints. Is there a way to only switch when needed?
              -- Probably run a EitherT inside.
              showApplications :: CocExpr -> IndentM [IndentData]
              showApplications (CocApply f a) = pass $ do
                -- Spy on single-line print
                (chunked, (_, Any newlines, Sum charCount)) <- listen $ do
                    revArgs <- showApplications f
                    appChar ' '
                    (_, curArg) <- listen (showHelper a)
                    return $ curArg : revArgs
                -- Capture and silence multi-line print
                (_, chunkedWrite) <- censor (const mempty) $ listen $ showMultiline chunked
                -- Pick appropriate output
                let outFunc = if newlines || charCount > 40
                    then const chunkedWrite
                    else id 
                return (chunked, outFunc)
              showApplications t = listen (showHelper t) >>= (\(_, x) -> return [x])

              showHelper :: CocExpr -> IndentM ()
              showHelper = \case
                CocProp -> "*"
                CocType -> "@"
                CocVariable index label -> fromString label
                CocHole label -> fromString label
                app@(CocApply f a) -> "(" <> void (withIndent $ showApplications app) <> ")"
                CocLambda p t b ->
                    "(\\" <> fromString p <> ":"
                        <> showHelper t <> "."
                        <> withIndent (indentedNewLine <> showHelper b)
                        <> ")"
                CocForall p t b -> case (p, isArrowExpr t) of
                    ("_", True) -> "(" <> withIndent (showHelper t) <> ")->"
                        <> showHelper b
                    ("_", False) -> showHelper t <> "->" <> showHelper b
                    _ -> "{\\" <> fromString p <> ":"
                        <> showHelper t <> "."
                        <> withIndent (showHelper b)
                        <> "}"


fromCocDefs :: [CocDefinition] -> Map.Map String CocExpr
fromCocDefs defs
    = foldl' f Map.empty defs
    where f defmap (CocDefinition defname expr)
            = Map.insert defname (fromCocSyntax defmap expr) defmap

data Scope = MkScope {
    defMap :: Map.Map String CocExpr,
    labelMap :: Map.Map String Int,
    depth :: Int
    } deriving (Show)

newtype ScopedM a = MkScopedM {
    runScoped :: ReaderT Scope (Except String) a
    } deriving newtype (Functor, Applicative, Monad,
        MonadReader Scope, MonadError String)

withCaptured :: String -> ScopedM a -> ScopedM a
withCaptured label = local $ \MkScope{..} -> MkScope defMap (Map.insert label (depth + 1) labelMap) $ depth + 1

withUnused :: ScopedM a -> ScopedM a
withUnused = local $ \MkScope{..} -> MkScope defMap labelMap $ depth + 1

lookupDef :: String -> ScopedM CocExpr
lookupDef label = asks (Map.lookup label . defMap) >>= \case
    Just expr -> return expr
    Nothing -> throwError $ "Error when parsing " ++ label
                    ++ ": variable not bound"

lookupVar :: String -> ScopedM Int
lookupVar label = asks (Map.lookup label . labelMap) >>= \case
    Just varDepth -> subtract varDepth <$> asks depth
    Nothing -> throwError $ "Error when parsing " ++ label
                    ++ ": variable not bound"

-- TODO: Throw error in app monad, when app monad is implemented
fromCocSyntax :: Map.Map String CocExpr -> CocSyntax -> CocExpr
fromCocSyntax defmap expr = case runExcept $ runReaderT (runScoped $ fromCocM expr) env of
    Left err -> error err
    Right val -> val
    where env = MkScope defmap Map.empty 0

fromCocM :: CocSyntax -> ScopedM CocExpr
fromCocM = \case
    CocSyntaxUnused -> throwError "Error when parsing _: Unused variable cannot appear in the bodies of expressions"
    CocSyntaxProp -> return CocProp
    CocSyntaxType -> return CocType
    CocSyntaxVariable label -> (CocVariable <$> lookupVar label <*> pure label)
        `catchError` const (lookupDef label)
    CocSyntaxHole label -> return $ CocHole label
    CocSyntaxApply func arg -> CocApply <$> fromCocM func <*> fromCocM arg
    CocSyntaxLambda{..} -> abstract CocLambda "lambda" param inType body
    CocSyntaxForall{..} -> abstract CocForall "forall" param inType body
    where abstract ctor name param inType body = case param of
            CocSyntaxVariable capture -> ctor capture
                <$> fromCocM inType <*> withCaptured capture (fromCocM body)
            CocSyntaxUnused -> ctor "_"
                <$> fromCocM inType <*> withUnused (fromCocM body)
            other -> throwError $ "Error when parsing " ++ show other
                        ++ ": invalid variable in" ++ name


asCocProp expr     | CocProp         <- expr = Just expr
asCocProp _                                  = Nothing
asCocType expr     | CocType         <- expr = Just expr
asCocType _                                  = Nothing
asCocVariable expr | CocVariable _ _ <- expr = Just expr
asCocVariable _                              = Nothing
asCocHole expr     | CocHole _       <- expr = Just expr
asCocHole _                                  = Nothing
asCocApply expr    | CocApply _ _    <- expr = Just expr
asCocApply _                                 = Nothing
asCocLambda expr   | CocLambda _ _ _ <- expr = Just expr
asCocLambda _                                = Nothing
asCocForall expr   | CocForall _ _ _ <- expr = Just expr
asCocForall _                                = Nothing
