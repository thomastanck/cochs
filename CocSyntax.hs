module CocSyntax where

import Data.List.Split(splitOn)

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
    -- Creates a definition binding
    | CocSyntaxDefine { defname :: String, expr :: CocSyntax }
    deriving Eq

isArrowSyntax (CocSyntaxForall CocSyntaxUnused _ _) = True
isArrowSyntax _ = False

instance Show CocSyntax where
    show (CocSyntaxProp) = "*"
    show (CocSyntaxType) = "@"
    show (CocSyntaxVariable label) = label
    show (CocSyntaxUnused) = "_"
    show (CocSyntaxApply function argument) = "(" ++ (show function) ++ " " ++ (show argument) ++ ")"
    show (CocSyntaxLambda param inType body) = "(\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ ")"
    show (CocSyntaxForall param inType body)
        = if param == CocSyntaxUnused
            then if isArrowSyntax inType
                then "(" ++ (show inType) ++ ")->" ++ (show body)
                else (show inType) ++ "->" ++ (show body)
            else "{\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ "}"
    show (CocSyntaxDefine defname expr) = "define " ++ defname ++ " = " ++ (show expr)

