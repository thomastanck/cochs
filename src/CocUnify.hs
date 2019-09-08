module CocUnify where

import Data.Map.Strict as Map

import CocExpr

unify :: CocExpr -> CocExpr -> Maybe (Map.Map String CocExpr)
unify exp1 exp2 = unifyeqns empty [(exp1, exp2)]

unifyeqns :: Map.Map String CocExpr -> [(CocExpr, CocExpr)] -> Maybe (Map.Map String CocExpr)
unifyeqns map [] = Just map
unifyeqns map ((CocVariable a _, CocVariable b _):eqns) = if a == b
                                                            then unifyeqns map eqns
                                                            else Nothing
unifyeqns map ((CocApply f a, CocApply g b):eqns) = unifyeqns map ([(f,g),(a,b)]++eqns)
unifyeqns map ((CocLambda _ a e, CocLambda _ b f):eqns) = unifyeqns map ([(a,b),(e,f)]++eqns)
unifyeqns map ((CocForall _ a e, CocForall _ b f):eqns) = unifyeqns map ([(a,b),(e,f)]++eqns)
unifyeqns map ((a, CocHole b):eqns) = unifyeqns map ([(CocHole b,a)]++eqns)
unifyeqns map ((CocHole a, b):eqns) = case (Map.lookup a map) of
                              Nothing -> if occurs map a b
                                then Nothing
                                else unifyeqns (insert a b map) eqns
                              Just a' -> unifyeqns map ([(a',b)]++eqns)
unifyeqns map ((a, b):eqns) = if a == b then unifyeqns map eqns else Nothing

occurs :: Map.Map String CocExpr -> String -> CocExpr -> Bool
occurs map a (CocHole b) = case Map.lookup b map of
                                        Nothing -> a == b
                                        Just b' -> occurs map a b'
occurs map a (CocApply f b) = occurs map a f || occurs map a b
occurs map a (CocLambda _ t b) = occurs map a t || occurs map a b
occurs map a (CocForall _ t b) = occurs map a t || occurs map a b
occurs map a _ = False
