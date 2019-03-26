module CocExpr where

import Data.List(elemIndex, foldl')
import Data.Maybe(fromMaybe)
import Data.Map.Strict as Map

import CocSyntax

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
    show (CocProp) = "*"
    show (CocType) = "@"
    show (CocVariable index label) = label ++ (show index)
    show (CocApply f a) = "(" ++ (show f) ++ " " ++ (show a) ++ ")"
    show (CocLambda p t b) = "(\\" ++ p ++ ":" ++ (show t) ++ "." ++ (show b) ++ ")"
    show (CocForall p t b)
        = if p == "_"
            then if isArrowExpr t
                then "(" ++ (show t) ++ ")->" ++ (show b)
                else (show t) ++ "->" ++ (show b)
            else "{\\" ++ p ++ ":" ++ (show t) ++ "." ++ (show b) ++ "}"

fromCocProgram :: (CocSyntax, [CocSyntax]) -> CocExpr
fromCocProgram (expr,defs) = fromCocSyntax (fromCocDefs defs) expr

fromCocDefs :: [CocSyntax] -> Map.Map String CocExpr
fromCocDefs defs
    = Data.List.foldl' (f :: Map.Map String CocExpr -> CocSyntax -> Map.Map String CocExpr) (Map.empty :: Map.Map String CocExpr) (defs :: [CocSyntax])
    where f defmap (CocSyntaxDefine defname expr)
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
asCocApply expr    | CocApply _ _    <- expr = Just expr
asCocApply _                                 = Nothing
asCocLambda expr   | CocLambda _ _ _ <- expr = Just expr
asCocLambda _                                = Nothing
asCocForall expr   | CocForall _ _ _ <- expr = Just expr
asCocForall _                                = Nothing
