module CocExpr where

import Data.List(elemIndex)
import Data.Maybe(fromMaybe)

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

instance Show CocExpr where
    show (CocProp) = "*"
    show (CocType) = "@"
    show (CocVariable index label) = label ++ (show index)
    show (CocApply f a) = "(" ++ (show f) ++ " " ++ (show a) ++ ")"
    show (CocLambda p t b) = "(\\" ++ p ++ ":" ++ (show t) ++ "." ++ (show b) ++ ")"
    show (CocForall p t b) = "{\\" ++ p ++ ":" ++ (show t) ++ "." ++ (show b) ++ "}"

fromCocSyntax :: CocSyntax -> CocExpr
fromCocSyntax syntax = fromCocSyntax' [] syntax

fromCocSyntax' :: [String] -> CocSyntax -> CocExpr
fromCocSyntax' labels syntax = case syntax of
    (CocSyntaxProp)
        -> CocProp
    (CocSyntaxType)
        -> CocType
    (CocSyntaxVariable label)
        -> case elemIndex label labels of
            Just i -> CocVariable i label
            Nothing -> error ("Error when parsing " ++ label ++ ": variable not bound")
    (CocSyntaxUnused)
        -> error ("Error when parsing _: Unused variable cannot appear in the bodies of expressions")
    (CocSyntaxApply function argument)
        -> CocApply (fromCocSyntax' labels function) (fromCocSyntax' labels argument)
    (CocSyntaxLambda (CocSyntaxVariable label) inType body)
        -> CocLambda label (fromCocSyntax' labels inType) (fromCocSyntax' (label:labels) body)
    (CocSyntaxLambda (CocSyntaxUnused) inType body)
        -> CocLambda "_" (fromCocSyntax' labels inType) (fromCocSyntax' ("_":labels) body)
    (CocSyntaxLambda other inType body)
        -> error ("Error when parsing " ++ (show other) ++ ": invalid variable in lambda")
    (CocSyntaxForall (CocSyntaxVariable label) inType body)
        -> CocForall label (fromCocSyntax' labels inType) (fromCocSyntax' (label:labels) body)
    (CocSyntaxForall (CocSyntaxUnused) inType body)
        -> CocForall "_" (fromCocSyntax' labels inType) (fromCocSyntax' ("_":labels) body)
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
