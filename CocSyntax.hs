module CocSyntax where

import Debug.Trace

data CocSyntax =
    -- Type of types
    -- e.g. CocForall (CocUnused) (CocVariable "A") (CocVariable "A")
    -- has type CocProp
    CocProp
    -- Type of CocProp
    | CocType
    -- Denotes a variable
    | CocVariable { label :: String }
    -- Can be used in place of an variable if it is unused
    | CocUnused
    -- Denotes an application
    | CocApply { function :: CocSyntax, argument :: CocSyntax }
    -- Creates a function abstraction
    | CocLambda { param :: CocSyntax, inType :: CocSyntax,  body :: CocSyntax }
    -- Creates a type abstraction
    | CocForall { param :: CocSyntax, inType :: CocSyntax,  body :: CocSyntax }

instance Eq CocSyntax where
    (==) CocProp CocProp =
        True
    (==) CocType CocType =
        True
    (==) (CocVariable a) (CocVariable b) =
        a == b
    (==) (CocUnused) (CocUnused) =
        True
    (==) (CocApply a b) (CocApply c d) =
        a == c
        && b == d
    (==) (CocLambda a b c) (CocLambda d e f) =
        b == e
        && (cocReplace a (CocVariable "1") c) == (cocReplace d (CocVariable "1") f)
    (==) (CocForall a b c) (CocForall d e f) =
        b == e
        && (cocReplace a (CocVariable "1") c) == (cocReplace d (CocVariable "1") f)
    (==) _ _ =
        False

instance Show CocSyntax where
    show (CocProp) = "*"
    show (CocType) = "@"
    show (CocVariable label) = label
    show (CocUnused) = "_"
    show (CocApply function argument) = "(" ++ (show function) ++ " " ++ (show argument) ++ ")"
    show (CocLambda param inType body) = "(\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ ")"
    show (CocForall param inType body) = "{\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ "}"

cocReplace :: CocSyntax -> CocSyntax -> CocSyntax -> CocSyntax
cocReplace CocUnused _ body = body
cocReplace variable replacement body
    | variable == body
    = replacement
cocReplace variable replacement (CocApply function argument) =
    (CocApply
     (cocReplace variable replacement function)
     (cocReplace variable replacement argument))
cocReplace variable replacement (CocLambda (CocVariable v) inType body) =
    if variable == (CocVariable v)
    then (CocLambda (CocVariable v) inType body)
    else (CocLambda
            (CocVariable v)
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocForall (CocVariable v) inType body) =
    if variable == (CocVariable v) then (CocForall (CocVariable v) inType body)
    else (CocForall
            (CocVariable v)
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocLambda otherParam inType body) =
    (CocLambda
            otherParam
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocForall otherParam inType body) =
    (CocForall
            otherParam
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body))
cocReplace variable replacement (CocVariable other) =
    CocVariable other
cocReplace variable replacement other =
    other
