module CocEval(cocEval) where

import Debug.Trace

import Data.Maybe

import CocSyntax

data CocSettings = CocSettings { typeDepOnTerm :: Bool, termDepOnType :: Bool, typeDepOnType :: Bool }

cocEval :: Int -> CocSyntax -> (CocSyntax, CocSyntax)
cocEval systemNum expr =
    cocEval' (CocSettings termDepOnType typeDepOnTerm typeDepOnType) expr
    where typeDepOnTerm = systemNum `mod` 2 == 1
          termDepOnType = systemNum `div` 2 `mod` 2 == 1
          typeDepOnType = systemNum `div` 4 `mod` 2 == 1

cocEval' :: CocSettings -> CocSyntax -> (CocSyntax, CocSyntax)
cocEval' settings expr =
    cocEvalLoop settings expr (expr, CocUnused Nothing)

cocEvalLoop :: CocSettings -> CocSyntax -> (CocSyntax, CocSyntax) -> (CocSyntax, CocSyntax)
cocEvalLoop settings expr prevExpr =
    trace ("    >>>> " ++ (show expr))
        (case reduce settings expr of
            Just result -> trace ("    <<<< " ++ (show result)) $ cocEvalLoop settings (fst result) result
            Nothing -> prevExpr
        )

rules = [
    reduce
    -- betaReduce,
    -- lambdaReduce,
    -- forallReduce,
    -- propReduce
    ]

-- betaReduce :: CocSettings -> CocSyntax -> Maybe (CocSyntax, CocSyntax)
-- betaReduce settings (CocApply (CocLambda param inType body) arg) =
--     Just $ (fst $ cocEval' settings $ cocReplace param arg body,
--             fst $ cocEval' settings $ cocReplace param inType body)
-- betaReduce _ _ =
--     Nothing

-- lambdaReduce :: CocSettings -> CocSyntax -> Maybe (CocSyntax, CocSyntax)
-- lambdaReduce settings (CocLambda param inType body) =
--     Just $ (CocLambda param inTypeVal bodyVal,
--             CocForall param inTypeVal bodyType)
--     where
--         (inTypeVal, _) = (cocEval' settings inType)
--         (bodyVal, bodyType) = (cocEval' settings body)
-- lambdaReduce _ _ =
--     Nothing

-- forallReduce :: CocSettings -> CocSyntax -> Maybe (CocSyntax, CocSyntax)
-- forallReduce settings (CocForall param inType body) =
--     Just $ (CocForall param inTypeVal bodyVal,
--             bodyType)
--     where
--         (inTypeVal, _) = (cocEval' settings inType)
--         (bodyVal, bodyType) = (cocEval' settings body)
-- forallReduce _ _ =
--     Nothing

nonsense = CocVariable "lmaoooooooooooooo" (Just $ CocVariable "AHHAHAHA" Nothing)

reduce :: CocSettings -> CocSyntax -> Maybe (CocSyntax, CocSyntax)
reduce settings CocProp =
    Just $ (CocProp,
            CocType)
reduce settings CocType =
    Just $ (CocType,
            CocUnused Nothing) -- A nonsense type so if this type is read it'll probably make no sense.
reduce settings (CocUnused _) =
    Nothing -- CocUnused shouldn't appear in a program
reduce settings (CocVariable v t) =
    Nothing
    -- Just $ (CocVariable v t,
    --         maybe nonsense id t) -- A nonsense type so if this type is read it'll probably make no sense.
reduce settings (CocApply (CocLambda param inType body _) arg _) =
    trace ("beta " ++ (show $ CocApply (CocLambda param inType body Nothing) arg Nothing))
        Just $ (result, CocUnused Nothing)
        where result = cocReplace param (withType arg inType) body
reduce settings (CocApply other arg t)
    | CocApply _ _ _ <- other = do
    (fun, _) <- reduce settings other
    Just $ (CocApply fun arg t, CocUnused Nothing)
reduce settings (CocApply other arg t) =
    Nothing
    -- error ("Application on non function " ++ (show (CocApply other arg t)))
reduce settings (CocLambda param inType body t) = do
    (inTypeVal, _) <- reduce settings inType
    (bodyVal, bodyType) <- reduce settings body
    let lambdaType = CocForall param inTypeVal bodyType Nothing
    Just $ (CocLambda param inTypeVal bodyVal (Just lambdaType),
            lambdaType)
reduce settings (CocForall param inType body t) = do
    (inTypeVal, _) <- reduce settings inType
    (bodyVal, bodyType) <- reduce settings (cocReplace param (withType param inType) body)
    Just $ (CocForall param inTypeVal bodyVal (Just bodyType),
            bodyType)
reduce _ _ =
    Nothing

--     >>>> ((\a:{\NatT:*.{\succ:{\_:NatT.NatT}.{\zero:NatT.NatT}}}.(\b:{\NatT:*.{\succ:{\_:NatT.NatT}.{\zero:NatT.NatT}}}.(\NatT:*.((b {\_:NatT.NatT}) (a NatT))))) (\NatT:*.(\succ:{\_:NatT.NatT}.(\zero:NatT.zero))))

--     >>>> (\b:{\NatT:*.{\succ:{\_:NatT.NatT}.{\zero:NatT.NatT}}}.(\NatT:*.((b {\_:NatT.NatT}) ((\NatT:*.(\succ:{\_:NatT.NatT}.(\zero:NatT.zero))) NatT))))

