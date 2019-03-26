module CocEval where

import Safe(atMay)

import CocExpr

data CocSort =
    CocSortProp
    | CocSortType
    deriving (Eq, Show)

asCocSort CocProp = Just CocSortProp
asCocSort CocType = Just CocSortType
asCocSort _ = Nothing

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

-- Finds the type of an expr in a given context
cocType :: CocSettings -> CocContext -> CocExpr -> Maybe CocExpr
cocType settings ctx expr
    | CocSettings allowedSorts <- settings
    = case expr of
        CocProp -> Just CocType
        CocType -> Nothing
        CocVariable index label -> atMay ctx index
        CocApply function argument -> do
            fType <- cocType settings ctx function
            (CocForall label inType body) <- Just $ cocNorm settings fType
            aType <- cocType settings ctx argument
            checkEqual (cocNorm settings inType) (cocNorm settings aType)
            Just $ cocNorm settings (cocSubst body 0 argument)
        CocLambda label inType body -> do
            inTypeType <- cocType settings ctx inType
            inTypeSort <- asCocSort inTypeType
            bodyType <- cocType settings (map (cocOpen 0) (inType:ctx)) body
            bodyTypeType <- cocType settings (map (cocOpen 0) (inType:ctx)) bodyType
            bodyTypeSort <- asCocSort bodyTypeType
            check $ (inTypeSort, bodyTypeSort) `elem` allowedSorts
            Just $ CocForall label (cocNorm settings inType) (cocNorm settings bodyType)
        CocForall label inType body -> do
            inTypeType <- cocType settings ctx inType
            inTypeSort <- asCocSort inTypeType
            bodyType <- cocType settings (map (cocOpen 0) (inType:ctx)) body
            bodySort <- asCocSort bodyType
            check $ (inTypeSort, bodySort) `elem` allowedSorts
            Just $ (cocNorm settings bodyType)
