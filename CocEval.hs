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
    (expr, expr)
