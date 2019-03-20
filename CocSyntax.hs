module CocSyntax where

import Debug.Trace

data CocSyntax =
    -- Type of types
    -- e.g. CocSyntaxForall (CocSyntaxUnused) (CocSyntaxVariable "A") (CocSyntaxVariable "A")
    -- has type CocSyntaxProp
    CocSyntaxProp
    -- Type of CocSyntaxProp
    | CocSyntaxType
    -- Denotes a variable
    | CocSyntaxVariable { label :: String }
    -- Can be used in place of an variable if it is unused
    | CocSyntaxUnused
    -- Denotes an application
    | CocSyntaxApply { function :: CocSyntax, argument :: CocSyntax }
    -- Creates a function abstraction
    | CocSyntaxLambda { param :: CocSyntax, inType :: CocSyntax,  body :: CocSyntax }
    -- Creates a type abstraction
    | CocSyntaxForall { param :: CocSyntax, inType :: CocSyntax,  body :: CocSyntax }

instance Eq CocSyntax where
    (==) CocSyntaxProp CocSyntaxProp =
        True
    (==) CocSyntaxType CocSyntaxType =
        True
    (==) (CocSyntaxVariable a) (CocSyntaxVariable b) =
        a == b
    (==) (CocSyntaxUnused) (CocSyntaxUnused) =
        True
    (==) (CocSyntaxApply a b) (CocSyntaxApply c d) =
        a == c
        && b == d
    (==) (CocSyntaxLambda a b c) (CocSyntaxLambda d e f) =
        b == e
        && (cocReplace a (CocSyntaxVariable "1") c) == (cocReplace d (CocSyntaxVariable "1") f)
    (==) (CocSyntaxForall a b c) (CocSyntaxForall d e f) =
        b == e
        && (cocReplace a (CocSyntaxVariable "1") c) == (cocReplace d (CocSyntaxVariable "1") f)
    (==) _ _ =
        False

instance Show CocSyntax where
    show (CocSyntaxProp) = "*"
    show (CocSyntaxType) = "@"
    show (CocSyntaxVariable label) = label
    show (CocSyntaxUnused) = "_"
    show (CocSyntaxApply function argument) = "(" ++ (show function) ++ " " ++ (show argument) ++ ")"
    show (CocSyntaxLambda param inType body) = "(\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ ")"
    show (CocSyntaxForall param inType body) = "{\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ "}"

cocReplace :: CocSyntax -> CocSyntax -> CocSyntax -> CocSyntax
cocReplace CocSyntaxUnused _ body = body
cocReplace variable replacement body
    | variable == body
    = replacement
cocReplace variable replacement (CocSyntaxApply function argument) =
    (CocSyntaxApply
     (cocReplace variable replacement function)
     (cocReplace variable replacement argument))
cocReplace variable replacement (CocSyntaxLambda (CocSyntaxVariable v) inType body) =
    if variable == (CocSyntaxVariable v)
    then (CocSyntaxLambda (CocSyntaxVariable v) inType body)
    else (CocSyntaxLambda
            (CocSyntaxVariable v)
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocSyntaxForall (CocSyntaxVariable v) inType body) =
    if variable == (CocSyntaxVariable v) then (CocSyntaxForall (CocSyntaxVariable v) inType body)
    else (CocSyntaxForall
            (CocSyntaxVariable v)
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocSyntaxLambda otherParam inType body) =
    (CocSyntaxLambda
            otherParam
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocSyntaxForall otherParam inType body) =
    (CocSyntaxForall
            otherParam
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocSyntaxVariable other) =
    CocSyntaxVariable other
cocReplace variable replacement other =
    other
