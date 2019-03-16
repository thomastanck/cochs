module CocParser(parseCocExpr) where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import CocExpr

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space C.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseCocExpr :: Parser CocExpr
parseCocExpr = do
    sc
    (try parseCocApply) <|> parseCocExprOne

parseCocExprOne :: Parser CocExpr
parseCocExprOne =
    parseCocProp
    <|> parseCocType
    <|> parseCocLambda
    <|> parseCocForall
    <|> parseCocParenthesised
    <|> parseCocVariable

parseCocProp :: Parser CocExpr
parseCocProp = do
    symbol "Prop" <|> symbol "*"
    return CocProp

parseCocType :: Parser CocExpr
parseCocType = do
    symbol "Type" <|> symbol "@"
    return CocType

parseCocApply :: Parser CocExpr
parseCocApply = do
    firstExpr <- parseCocExprOne
    exprs <- some parseCocExprOne
    return $ foldr (\a b->CocApply b a Nothing) firstExpr (reverse exprs)

parseCocLambda :: Parser CocExpr
parseCocLambda = do
    symbol "(\\"
    param <- option (CocUnused Nothing) (parseCocUnused <|> parseCocVariable)
    symbol ":"
    intype <- parseCocExpr
    symbol "."
    body <- parseCocExpr
    symbol ")"
    return $ CocLambda param intype body Nothing

parseCocForall :: Parser CocExpr
parseCocForall = do
    symbol "{\\"
    param <- option (CocUnused Nothing) (parseCocUnused <|> parseCocVariable)
    symbol ":"
    intype <- parseCocExpr
    symbol "."
    body <- parseCocExpr
    symbol "}"
    return $ CocForall param intype body Nothing

parseCocParenthesised :: Parser CocExpr
parseCocParenthesised = do
    symbol "("
    a <- parseCocExpr
    symbol ")"
    return a

parseCocUnused :: Parser CocExpr
parseCocUnused = do
    symbol "_"
    return $ CocUnused Nothing

rws :: [String] -- list of reserved words
rws = ["Prop", "*", "Type", "@", "_"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> C.letterChar <*> many C.alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

parseCocVariable :: Parser CocExpr
parseCocVariable = do
    ident <- identifier
    return $ CocVariable ident Nothing
