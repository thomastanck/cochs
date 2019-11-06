module CocSyntax where

import Data.List.Split(splitOn)
import Data.List

data CocSyntax =
    -- Type of types
    -- e.g. CocSyntaxForall (CocSyntaxUnused) (CocSyntaxVariable "A") (CocSyntaxVariable "A")
    -- has type CocSyntaxProp
    CocSyntaxProp
    -- Type of CocSyntaxProp
    | CocSyntaxType
    -- Denotes a variable
    | CocSyntaxVariable { label :: String }
    -- Denotes a hole
    | CocSyntaxHole { label :: String }
    -- Can be used in place of an variable if it is unused
    | CocSyntaxUnused
    -- Denotes an application
    | CocSyntaxApply { function :: CocSyntax, argument :: CocSyntax }
    -- Creates a function abstraction
    | CocSyntaxLambda { param :: CocSyntax, inType :: CocSyntax,  body :: CocSyntax }
    -- Creates a type abstraction
    | CocSyntaxForall { param :: CocSyntax, inType :: CocSyntax,  body :: CocSyntax }
    deriving Eq

isArrowSyntax (CocSyntaxForall CocSyntaxUnused _ _) = True
isArrowSyntax _ = False

instance Show CocSyntax where
    showsPrec prec val = case val of
        CocSyntaxProp -> ('*':)
        CocSyntaxType -> ('@':)
        CocSyntaxVariable label -> (label ++)
        CocSyntaxHole label -> (label ++)
        CocSyntaxUnused -> ('_':)
        CocSyntaxApply function argument -> ('(':) . shows function . (' ':) . shows argument . ('(':)
        CocSyntaxLambda param inType body -> ("(\\" ++) . shows param . (':':) . shows inType . ('.':) . shows body . (')':)
        CocSyntaxForall param inType body ->
            if param == CocSyntaxUnused
                then if isArrowSyntax inType
                    then ('(':) . shows inType . (")->" ++) . shows body
                    else shows inType . ("->" ++) . shows body
                else ("{\\" ++) . shows param . (":" ++)
                        . shows inType . ('.':)
                        . shows body . ('}':)
        where shows = showsPrec prec


-- Creates a definition binding
data CocDefinition = CocDefinition { defname :: String, expr :: CocSyntax }
    deriving Eq

instance Show CocDefinition where
    show (CocDefinition defname expr) = "define " ++ defname ++ " = " ++ show expr

data CocImport = CocImport { packagename :: String, defnamemap :: [(String, String)] }

instance Show CocImport where
    show (CocImport packagename defnamemap) = "import " ++ packagename ++ "{" ++ unwords defnames ++ "}"
        where defnames = map defname defnamemap
              defname (a,b) = if a == b then a else a ++ " as " ++ b
