{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CocEval where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import Control.Parallel
import Safe(atMay)

import CocExpr

newtype CocSettings = CocSettings { allowedSorts :: [(CocSort, CocSort)] }
    deriving (Eq, Show)

type CocContext = [CocType]

systemNumToSettings :: Int -> CocSettings
systemNumToSettings systemNum = CocSettings (propprop ++ proptype ++ typeprop ++ typetype)
    where propprop = [(CocSortProp, CocSortProp)]
          proptype = [(CocSortProp, CocSortType) |  systemNum `mod` 2 == 1]
          typeprop = [(CocSortType, CocSortProp) |  systemNum `div` 2 `mod` 2 == 1]
          typetype = [(CocSortType, CocSortType) |  systemNum `div` 4 `mod` 2 == 1]

checkEqual a b = if a == b then Just a else Nothing
check b = if b then Just b else Nothing

-- Finds the normal form of an expr (without typechecking)
cocNorm :: CocExpr -> Reader CocSettings CocExpr
cocNorm = norm
    where norm expr = case expr of
            CocApply function argument -> CocApply <$> norm function <*> norm argument >>= \case
                CocApply (CocLambda _ inType body) arg -> norm $ cocSubst body 0 arg
                other -> return other
            CocLambda label inType body -> CocLambda label <$> norm inType <*> norm body
            CocForall label inType body -> CocForall label <$> norm inType <*> norm body
            _ -> return expr

cocSubst :: CocExpr -> Int -> CocExpr -> CocExpr
cocSubst expr index withExpr
    = case expr of
        CocProp -> expr
        CocType -> expr
        CocVariable varindex label -> case compare varindex index of
            LT -> CocVariable varindex label
            EQ -> withExpr
            GT -> CocVariable (varindex-1) label
        CocApply function argument -> CocApply (cocSubst function index withExpr) (cocSubst argument index withExpr)
        CocLambda label inType body -> CocLambda label (cocSubst inType index withExpr) (cocSubst body (index+1) (cocOpen 0 withExpr))
        CocForall label inType body -> CocForall label (cocSubst inType index withExpr) (cocSubst body (index+1) (cocOpen 0 withExpr))

cocOpen :: Int -> CocExpr -> CocExpr
cocOpen index expr
    = case expr of
        CocProp -> expr
        CocType -> expr
        CocVariable varindex label -> case compare varindex index of
            LT -> CocVariable varindex label
            _ -> CocVariable (varindex+1) label
        CocApply function argument -> CocApply (cocOpen index function) (cocOpen index argument)
        CocLambda label inType body -> CocLambda label (cocOpen index inType) (cocOpen (index+1) body)
        CocForall label inType body -> CocForall label (cocOpen index inType) (cocOpen (index+1) body)

data CocError
    = CocTypeMismatch { expr :: CocExpr, expectedType :: CocType, gotType :: CocType }
    | CocTypeHasNoType
    | CocVariableNotBound { index :: Int, label :: String }
    | CocNotSortError { expr :: CocExpr, type' :: CocType }
    | CocNonFunctionApplication { expr :: CocExpr, type' :: CocType }

instance Show CocError where
    show (CocTypeMismatch expr expectedType gotType)
        = "Type mismatch in expression:\n"
        ++ (show expr)
        ++ "\nExpected type:\n"
        ++ (show expectedType)
        ++ "\nGot type:\n"
        ++ (show gotType)
    show (CocTypeHasNoType)
        = "Tried to obtain type of Type, but Type has no type."
    show (CocVariableNotBound index label)
        = "Tried to find type of variable " ++ label ++ ", but it is not bound."
    show (CocNotSortError expr type')
        = "Expected " ++ (show expr) ++ " to be a type, but its type is " ++ (show type') ++ "which is not a valid sort."
    show (CocNonFunctionApplication expr type')
        = "Expected " ++ (show expr) ++ " to be a function (aka have a Forall type), but its type is " ++ (show type') ++ " which is not a Forall type."

newtype EvalM r a = MkEval {
    runEvalM :: ReaderT (CocSettings, r) (Except CocError) a
    } deriving (Functor, Applicative, Monad, MonadReader (CocSettings, r), MonadError CocError)


data CocSort =
    CocSortProp
    | CocSortType
    deriving (Eq, Show)

asCocSort CocProp _ = return CocSortProp
asCocSort CocType _ = return CocSortType
asCocSort _ err = throwError err


-- Finds the type of an expr in a given context
cocType :: CocSettings -> CocExpr -> Either CocError CocExpr
cocType settings expr = runExcept $ runReaderT (runEvalM $ go expr) (settings, [])
    where shiftOpen :: CocExpr -> EvalM CocContext a -> EvalM CocContext a
          shiftOpen pushed = local $ \(settings, ctx) -> (settings, map (cocOpen 0) (pushed:ctx))

          calcTypeSort expr = do
            ty <- go expr
            sort <- asCocSort ty $ CocNotSortError expr ty
            return (ty, sort)

          liftNorm :: Reader CocSettings a -> EvalM r a
          liftNorm = MkEval . mapReaderT (return . runIdentity) . withReaderT fst
          cocNorm' = liftNorm . cocNorm

          guardSort :: CocSort -> CocSort -> EvalM r ()
          guardSort inTypeSort bodySort = do
              valid <- asks $ elem (inTypeSort, bodySort) . allowedSorts . fst
              unless valid $ throwError CocTypeHasNoType
          go :: CocExpr -> EvalM CocContext CocExpr
          go expr = case expr of
            CocProp -> return CocType
            CocType -> throwError CocTypeHasNoType
            CocVariable index label -> do
                ctx <- asks snd
                case atMay ctx index of
                    Just t -> return t
                    Nothing -> throwError $ CocVariableNotBound index label
            CocApply function argument -> do
                fType <- go function
                normfType <- cocNorm' fType
                case normfType of
                    CocForall label inType body -> do
                        aType <- go argument
                        -- Run only in the Reader monad to avoid error side effects from blocking concurrency
                        (inTypeNorm, aTypeNorm, bodyNorm) <- liftNorm $ do
                            inTypeNorm <- cocNorm inType
                            aTypeNorm <- cocNorm aType
                            bodyNorm <- cocNorm (cocSubst body 0 argument)
                            let inTypeNorm' = bodyNorm `par` inTypeNorm `par` aTypeNorm `pseq` inTypeNorm
                            return (inTypeNorm', aTypeNorm, bodyNorm)
                        if inTypeNorm == aTypeNorm
                            then return bodyNorm
                            else throwError $ CocTypeMismatch argument inType aType
                    _ -> throwError $ CocNonFunctionApplication function normfType
            CocLambda label inType body -> do
                (_, inTypeSort) <- calcTypeSort inType
                shiftOpen inType $ do
                    bodyType <- go body
                    (_, bodyTypeSort) <- calcTypeSort bodyType
                    guardSort inTypeSort bodyTypeSort
                    -- Similarly, lift outside the two evaluations
                    liftNorm $ CocForall label <$> cocNorm inType <*> cocNorm bodyType
            CocForall label inType body -> do
                (_, inTypeSort) <- calcTypeSort inType
                (bodyType, bodySort) <- shiftOpen inType $ calcTypeSort body
                guardSort inTypeSort bodySort
                cocNorm' bodyType

