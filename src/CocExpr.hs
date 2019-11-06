{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CocExpr where

import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

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
                    ("_", True) -> "(" <> withIndent (showHelper t) <> ")->" <> showHelper b
                    ("_", False) -> showHelper t <> "->" <> showHelper b
                    _ -> "{\\" <> fromString p <> ":"
                        <> showHelper t <> "."
                        <> withIndent (showHelper b)
                        <> "}"


fromCocDefs :: [CocDefinition] -> Map.Map String CocExpr
fromCocDefs defs
    = Data.List.foldl' f Map.empty defs
    where f defmap (CocDefinition defname expr)
            = insert defname (fromCocSyntax defmap expr) defmap

fromCocSyntax :: Map.Map String CocExpr -> CocSyntax -> CocExpr
fromCocSyntax defmap syntax = fromCocSyntax' defmap [] syntax

fromCocSyntax' :: Map.Map String CocExpr -> [String] -> CocSyntax -> CocExpr
fromCocSyntax' defmap labels syntax = case syntax of
    (CocSyntaxProp)
        -> CocProp
    (CocSyntaxType)
        -> CocType
    (CocSyntaxVariable label)
        -> case elemIndex label labels of
            Just i -> CocVariable i label
            Nothing -> case Map.lookup label defmap of
                Just expr -> expr
                Nothing -> error ("Error when parsing " ++ label ++ ": variable not bound")
    (CocSyntaxHole label)
        -> CocHole label
    (CocSyntaxUnused)
        -> error ("Error when parsing _: Unused variable cannot appear in the bodies of expressions")
    (CocSyntaxApply function argument)
        -> CocApply (fromCocSyntax' defmap labels function) (fromCocSyntax' defmap labels argument)
    (CocSyntaxLambda (CocSyntaxVariable label) inType body)
        -> CocLambda label (fromCocSyntax' defmap labels inType) (fromCocSyntax' defmap (label:labels) body)
    (CocSyntaxLambda (CocSyntaxUnused) inType body)
        -> CocLambda "_" (fromCocSyntax' defmap labels inType) (fromCocSyntax' defmap ("_":labels) body)
    (CocSyntaxLambda other inType body)
        -> error ("Error when parsing " ++ (show other) ++ ": invalid variable in lambda")
    (CocSyntaxForall (CocSyntaxVariable label) inType body)
        -> CocForall label (fromCocSyntax' defmap labels inType) (fromCocSyntax' defmap (label:labels) body)
    (CocSyntaxForall (CocSyntaxUnused) inType body)
        -> CocForall "_" (fromCocSyntax' defmap labels inType) (fromCocSyntax' defmap ("_":labels) body)
    (CocSyntaxForall other inType body)
        -> error ("Error when parsing " ++ (show other) ++ ": invalid variable in forall")

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
