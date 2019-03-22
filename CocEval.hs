module CocEval where

import Debug.Trace

import Safe(atMay)
import Data.Maybe

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

data CocFTRule =
    CocFTRuleProp
    | CocFTRuleVar
    | CocFTRuleApply
    | CocFTRuleLambda { s1 :: CocSort, s2 :: CocSort }
    | CocFTRuleForall { s1 :: CocSort, s2 :: CocSort }
    deriving (Eq, Show)

data CocJudgement = CocJudgement { termContext :: [CocExpr], term :: CocExpr, typeContext :: [CocExpr], type' :: CocExpr }
    deriving (Eq, Show)

type CocContext = [CocJudgement]

systemNumToSettings :: Int -> CocSettings
systemNumToSettings systemNum = CocSettings (propprop ++ proptype ++ typeprop ++ typetype)
    where propprop = [(CocSortProp, CocSortProp)]
          proptype = if systemNum `mod` 2 == 1 then [(CocSortProp, CocSortType)] else []
          typeprop = if systemNum `div` 2 `mod` 2 == 1 then [(CocSortType, CocSortProp)] else []
          typetype = if systemNum `div` 4 `mod` 2 == 1 then [(CocSortType, CocSortType)] else []

cocEval :: Int -> CocExpr -> CocJudgement
cocEval systemNum expr =
    cocEval' (systemNumToSettings systemNum) expr

cocEval' :: CocSettings -> CocExpr -> CocJudgement
cocEval' settings expr =
    CocJudgement [] expr [] expr

traceme a = trace (show a) Just a

checkEqual a b = if a == b then Just a else trace ((show a) ++ " != " ++ (show b)) Nothing

extendFTS (FTS rules context) x = (FTS rules (x:context))

ftsWithContext (FTS rules _) context = (FTS rules context)

cocShift :: [CocExpr] -> [CocExpr] -> CocExpr -> CocExpr
cocShift fromCtx toCtx expr = cocShift' ((length toCtx) - (length fromCtx)) expr

cocShift' shiftAmt CocProp = CocProp
cocShift' shiftAmt CocType = CocType
cocShift' shiftAmt (CocVariable index label) = (CocVariable (index+shiftAmt) label)
cocShift' shiftAmt (CocApply f a) = (CocApply (cocShift' shiftAmt f) (cocShift' shiftAmt a))
cocShift' shiftAmt (CocLambda l t b) = (CocLambda l (cocShift' shiftAmt t) (cocShift' shiftAmt b))
cocShift' shiftAmt (CocForall l t b) = (CocForall l (cocShift' shiftAmt t) (cocShift' shiftAmt b))

applyFTRule :: CocFindTypeState -> CocFTRule -> CocExpr -> Maybe CocJudgement
applyFTRule fts (CocFTRuleProp) term
    | (CocProp) <- term
    , (FTS rules context) <- fts
    = Just $ CocJudgement context term [] CocType
applyFTRule fts (CocFTRuleVar) term
    | (CocVariable index varlabel) <- term
    , (FTS rules context) <- fts
    = trace ("VAR " ++ (show context) ++ " " ++ (show varlabel) ++ (show index) ++ " = " ++ (show $ context !! index)) (do
        varType <- atMay context index
        return $ CocJudgement context term (drop (index+1) context) varType)
applyFTRule fts (CocFTRuleApply) term
    | (CocApply function argument) <- term
    , (FTS rules context) <- fts
    = do
        trace ("((((((( " ++ (show context) ++ " of " ++ (show term)) (Just term)
        (CocJudgement _ _ typeContext functionType) <- cocFindType fts function
        traceme functionType
        (CocForall _ inType body) <- asCocForall functionType
        traceme "hithere"
        (CocJudgement _ _ _ argumentType) <- cocFindType fts argument
        traceme inType
        traceme argumentType
        traceme $ checkEqual inType argumentType
        checkEqual inType argumentType
        return $ CocJudgement context term (inType:typeContext) body
applyFTRule fts (CocFTRuleLambda s1 s2) term
    | (CocLambda label inType body) <- term
    , (FTS rules context) <- fts
    = do
        trace (">>> " ++ (show context) ++ " of " ++ (show term)) (Just body)
        trace (show s1) (Just body)
        trace (show s2) (Just body)
        traceme (CocLambda label inType body)
        (CocJudgement _ _ _ inTypeType) <- cocFindType fts inType
        trace (show s1) (Just body)
        trace (show s2) (Just body)
        traceme inType
        traceme inTypeType
        inTypeSort <- asCocSort inTypeType
        checkEqual inTypeSort s1
        trace (show s1) (Just body)
        trace (show s2) (Just body)
        traceme inTypeSort
        (CocJudgement _ _ bodyTypeContext bodyType) <- cocFindType (extendFTS fts inType) body
        traceme "--------1"
        traceme bodyType
        (CocJudgement _ _ _ bodyTypeType) <- cocFindType (ftsWithContext fts bodyTypeContext) bodyType
        traceme "--------2"
        traceme term
        traceme bodyType
        traceme bodyTypeType
        bodyTypeSort <- asCocSort bodyTypeType
        traceme "--------3"
        trace (show s1) (Just body)
        trace (show s2) (Just body)
        traceme bodyType
        traceme bodyTypeType
        traceme bodyTypeSort
        checkEqual bodyTypeSort s2
        return $ CocJudgement context term context (CocForall label inType (cocShift' 1 (cocShift bodyTypeContext context bodyType)))
applyFTRule fts (CocFTRuleForall s1 s2) term
    | (CocForall _ inType body) <- term
    , (FTS rules context) <- fts
    = do
        trace ("}}}}}}} " ++ (show term)) (Just 1)
        (CocJudgement _ _ _ inTypeType) <- cocFindType fts inType
        inTypeSort <- asCocSort inTypeType
        checkEqual inTypeSort s1
        (CocJudgement _ _ _ bodyType) <- cocFindType (extendFTS fts inType) body
        bodySort <- asCocSort bodyType
        checkEqual bodySort s2
        return $ CocJudgement context term context bodyType
applyFTRule _ _ _
    = Nothing

data CocFindTypeState = FTS { rules :: [CocFTRule], context :: [CocExpr] }
    deriving (Eq, Show)

cocInitFindTypeState :: CocSettings -> CocFindTypeState
cocInitFindTypeState (CocSettings allowedSorts)
    = FTS
        ([CocFTRuleProp,
          CocFTRuleVar,
          CocFTRuleApply]
         ++ (allowedSorts >>= (\(s1, s2) -> [CocFTRuleLambda s1 s2, CocFTRuleForall s1 s2]))
        )
        []

cocFindType :: CocFindTypeState -> CocExpr -> Maybe CocJudgement
cocFindType fts expr
    | FTS rules context <- fts
    = atMay (do
    rule <- rules
    judgement <- maybeToList $ applyFTRule fts rule expr
    trace "Hiiiiiiiiiiiiiiiiiiii" [1]
    trace (show judgement) [1]
    return judgement) 0
