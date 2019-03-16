module CocExpr where

import Debug.Trace

data CocExpr =
    -- Type of types
    -- e.g. CocForall (CocUnused) (CocVariable "A") (CocVariable "A")
    -- has type CocProp
    CocProp
    -- Type of CocProp
    | CocType
    -- Denotes a variable
    | CocVariable { label :: String, type' :: Maybe CocExpr }
    -- Can be used in place of an variable if it is unused
    | CocUnused { type' :: Maybe CocExpr }
    -- Denotes an application
    | CocApply { function :: CocExpr, argument :: CocExpr, type' :: Maybe CocExpr }
    -- Creates a function abstraction
    | CocLambda { param :: CocExpr, inType :: CocExpr,  body :: CocExpr, type' :: Maybe CocExpr }
    -- Creates a type abstraction
    | CocForall { param :: CocExpr, inType :: CocExpr,  body :: CocExpr, type' :: Maybe CocExpr }

instance Eq CocExpr where
    (==) CocProp CocProp =
        True
    (==) CocType CocType =
        True
    (==) (CocVariable a _) (CocVariable b _) =
        a == b
    (==) (CocUnused _) (CocUnused _) =
        True
    (==) (CocApply a b _) (CocApply c d _) =
        a == c
        && b == d
    (==) (CocLambda a b c _) (CocLambda d e f _) =
        b == e
        && (cocReplace a (CocVariable "1" Nothing) c) == (cocReplace d (CocVariable "1" Nothing) f)
    (==) (CocForall a b c _) (CocForall d e f _) =
        b == e
        && (cocReplace a (CocVariable "1" Nothing) c) == (cocReplace d (CocVariable "1" Nothing) f)
    (==) _ _ =
        False

instance Show CocExpr where
    show (CocProp) = "*"
    show (CocType) = "@"
    show (CocVariable label type') = label -- ++ (maybe "" (\t -> ":" ++ (show t)) type')
    show (CocUnused type') = "_" -- ++ (maybe "" (\t -> ":" ++ (show t)) type')
    show (CocApply function argument type') = "(" ++ (show function) ++ " " ++ (show argument) ++ ")" -- ++ (maybe "" (\t -> ":" ++ (show t)) type')
    show (CocLambda param inType body type') = "(\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ ")" -- ++ (maybe "" (\t -> ":" ++ (show t)) type')
    show (CocForall param inType body type') = "{\\" ++ (show param) ++ ":" ++ (show inType) ++ "." ++ (show body) ++ "}" -- ++ (maybe "" (\t -> ":" ++ (show t)) type')

withType :: CocExpr -> CocExpr -> CocExpr
withType CocProp _ = CocProp
withType CocType _ = CocType
withType (CocVariable label Nothing) newType = (CocVariable label (Just newType))
withType (CocUnused Nothing) newType = (CocUnused (Just newType))
withType (CocApply function argument Nothing) newType = (CocApply function argument (Just newType))
withType (CocLambda param inType body Nothing) newType = (CocLambda param inType body (Just newType))
withType (CocForall param inType body Nothing) newType = (CocForall param inType body (Just newType))
withType (CocVariable label (Just t)) newType = typecheck newType t (CocVariable label (Just t))
withType (CocUnused (Just t)) newType = typecheck newType t (CocUnused (Just t))
withType (CocApply function argument (Just t)) newType = typecheck newType t (CocApply function argument (Just t))
withType (CocLambda param inType body (Just t)) newType = typecheck newType t (CocLambda param inType body (Just t))
withType (CocForall param inType body (Just t)) newType = typecheck newType t (CocForall param inType body (Just t))

typecheck newType t expr =
    if newType /= t then
        error ("Typechecking failed. Expected " ++ (show newType) ++ " but got " ++ (show t))
    else
        expr

getType :: CocExpr -> CocExpr
getType CocProp = CocType
getType (CocVariable _ (Just t)) = t
getType (CocUnused (Just t)) = t
getType (CocApply _ _ (Just t)) = t
getType (CocLambda _ _ _ (Just t)) = t
getType (CocForall _ _ _ (Just t)) = t
getType _ = notype

notype = CocVariable "notype" Nothing

cocReplace :: CocExpr -> CocExpr -> CocExpr -> CocExpr
cocReplace (CocUnused _) _ body = body
cocReplace variable replacement body
    | variable == body
    = trace ("REPLACING " ++ (show variable) ++ " WITH " ++ (show replacement) ++ " as it matches " ++ (show body)) replacement
cocReplace variable replacement (CocApply function argument t) =
    trace "apply" (CocApply
     (cocReplace variable replacement function)
     (cocReplace variable replacement argument)
     t)
cocReplace variable replacement (CocLambda (CocVariable v t1) inType body t) =
    if variable == (CocVariable v t1) then trace ("shadowing " ++ (show variable) ++ v) (CocLambda (CocVariable (v) t1) inType body t)
    else trace ("going in " ++ (show variable) ++ (show replacement) ++ (show $ CocLambda (CocVariable v t1) inType body t)) (CocLambda
            (CocVariable v t1)
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body)
            t)
cocReplace variable replacement (CocForall (CocVariable v t1) inType body t) =
    if variable == (CocVariable v t1) then trace "hmm" (CocForall (CocVariable (v) t1) inType body t)
    else trace "hmm" (CocForall
            (CocVariable v t1)
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body)
            t)
cocReplace variable replacement (CocLambda otherParam inType body t) =
    trace ("going in " ++ (show variable) ++ (show replacement) ++ (show (CocLambda otherParam inType body t))) (CocLambda
            otherParam
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body)
            t)
cocReplace variable replacement (CocForall otherParam inType body t) =
    trace "hmm" (CocForall
            otherParam
            (cocReplace variable replacement inType)
            (cocReplace variable replacement body)
            t)
cocReplace variable replacement (CocVariable other otherType) =
    trace ("not replacing " ++ (show variable) ++ " as it doesn't match " ++ (show $ CocVariable other otherType)) CocVariable (other) otherType
cocReplace variable replacement other =
    trace ("not replacing " ++ (show variable) ++ " as it didn't match any pattern and doesn't match " ++ (show other)) other
