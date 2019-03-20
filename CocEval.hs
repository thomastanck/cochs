module CocEval where

import Debug.Trace

import Data.Maybe

import CocExpr

data CocSettings = CocSettings { typeDepOnTerm :: Bool, termDepOnType :: Bool, typeDepOnType :: Bool }

data CocJudgement = CocJudgement { term :: CocExpr, type' :: CocExpr }

type CocContext = [CocJudgement]

cocEval :: Int -> CocExpr -> CocJudgement
cocEval systemNum expr =
    cocEval' (CocSettings termDepOnType typeDepOnTerm typeDepOnType) expr
    where typeDepOnTerm = systemNum `mod` 2 == 1
          termDepOnType = systemNum `div` 2 `mod` 2 == 1
          typeDepOnType = systemNum `div` 4 `mod` 2 == 1

cocEval' :: CocSettings -> CocExpr -> CocJudgement
cocEval' settings expr =
    CocJudgement expr expr
