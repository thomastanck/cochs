module CocEval where

import Control.Parallel
import Safe(atMay)

import CocExpr

data CocSort =
    CocSortProp
    | CocSortType
    deriving (Eq, Show)

asCocSort CocProp _ = Right CocSortProp
asCocSort CocType _ = Right CocSortType
asCocSort _ err = Left err

data CocSettings = CocSettings { allowedSorts :: [(CocSort, CocSort)] }
    deriving (Eq, Show)

type CocContext = [CocType]

systemNumToSettings :: Int -> CocSettings
systemNumToSettings systemNum = CocSettings (propprop ++ proptype ++ typeprop ++ typetype)
    where propprop = [(CocSortProp, CocSortProp)]
          proptype = if systemNum `mod` 2 == 1 then [(CocSortProp, CocSortType)] else []
          typeprop = if systemNum `div` 2 `mod` 2 == 1 then [(CocSortType, CocSortProp)] else []
          typetype = if systemNum `div` 4 `mod` 2 == 1 then [(CocSortType, CocSortType)] else []

checkEqual a b = if a == b then Just a else Nothing
check b = if b then Just b else Nothing

-- Finds the normal form of an expr (without typechecking)
cocNorm :: CocSettings -> CocExpr -> CocExpr
cocNorm settings expr
    | CocSettings allowedSorts <- settings
    = case expr of
        CocProp -> expr
        CocType -> expr
        CocVariable index label -> expr
        CocApply function argument -> case (CocApply (cocNorm settings function) (cocNorm settings argument)) of
            CocApply (CocLambda _ inType body) normArgument -> cocNorm settings (cocSubst body 0 normArgument)
            other -> other
        CocLambda label inType body -> CocLambda label (cocNorm settings inType) (cocNorm settings body)
        CocForall label inType body -> CocForall label (cocNorm settings inType) (cocNorm settings body)

cocSubst :: CocExpr -> Int -> CocExpr -> CocExpr
cocSubst expr index withExpr
    = case expr of
        CocProp -> expr
        CocType -> expr
        CocVariable varindex label -> case (compare varindex index) of
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
        CocVariable varindex label -> case (compare varindex index) of
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

-- Finds the type of an expr in a given context
cocType :: CocSettings -> CocContext -> CocExpr -> Either CocError CocExpr
cocType settings ctx expr
    | CocSettings allowedSorts <- settings
    = case expr of
        CocProp -> Right CocType
        CocType -> Left CocTypeHasNoType
        CocVariable index label -> case atMay ctx index of
            Just t -> Right t
            Nothing -> Left (CocVariableNotBound index label)
        CocApply function argument -> do
            fType <- cocType settings ctx function
            let normfType = cocNorm settings fType
            case normfType of
                (CocForall label inType body)
                    -> do aType <- cocType settings ctx argument
                          let inTypeNorm = cocNorm settings inType
                          let aTypeNorm = cocNorm settings aType
                          let bodyNorm = cocNorm settings (cocSubst body 0 argument)
                          if bodyNorm `par` inTypeNorm `par` (aTypeNorm `pseq` inTypeNorm == aTypeNorm)
                              then Right bodyNorm
                              else Left (CocTypeMismatch argument inType aType)
                _ -> Left (CocNonFunctionApplication function normfType)
        CocLambda label inType body -> do
            inTypeType <- cocType settings ctx inType
            inTypeSort <- asCocSort inTypeType (CocNotSortError inType inTypeType)
            bodyType <- cocType settings (map (cocOpen 0) (inType:ctx)) body
            bodyTypeType <- cocType settings (map (cocOpen 0) (inType:ctx)) bodyType
            bodyTypeSort <- asCocSort bodyTypeType (CocNotSortError bodyType bodyTypeType)
            if (inTypeSort, bodyTypeSort) `elem` allowedSorts
                then Right $ CocForall label (cocNorm settings inType) (cocNorm settings bodyType)
                else Left CocTypeHasNoType
        CocForall label inType body -> do
            inTypeType <- cocType settings ctx inType
            inTypeSort <- asCocSort inTypeType (CocNotSortError inType inTypeType)
            bodyType <- cocType settings (map (cocOpen 0) (inType:ctx)) body
            bodySort <- asCocSort bodyType (CocNotSortError body bodyType)
            if (inTypeSort, bodySort) `elem` allowedSorts
                then Right $ (cocNorm settings bodyType)
                else Left CocTypeHasNoType
