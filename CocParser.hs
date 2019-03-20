module CocParser(parseCocSyntax) where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import CocSyntax

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

parseCocSyntax :: Parser CocSyntax
parseCocSyntax = do
    sc
    (try parseCocApply) <|> parseCocSyntaxOne

parseCocSyntaxOne :: Parser CocSyntax
parseCocSyntaxOne =
    parseCocProp
    <|> parseCocType
    <|> parseCocLambda
    <|> parseCocForall
    <|> parseCocParenthesised
    <|> parseCocVariable

parseCocProp :: Parser CocSyntax
parseCocProp = do
    symbol "Prop" <|> symbol "*"
    return CocProp

parseCocType :: Parser CocSyntax
parseCocType = do
    symbol "Type" <|> symbol "@"
    return CocType

parseCocApply :: Parser CocSyntax
parseCocApply = do
    firstExpr <- parseCocSyntaxOne
    exprs <- some parseCocSyntaxOne
    return $ foldr (\a b->CocApply b a) firstExpr (reverse exprs)

parseCocLambda :: Parser CocSyntax
parseCocLambda = do
    symbol "(\\"
    param <- option CocUnused (parseCocUnused <|> parseCocVariable)
    symbol ":"
    intype <- parseCocSyntax
    symbol "."
    body <- parseCocSyntax
    symbol ")"
    return $ CocLambda param intype body

parseCocForall :: Parser CocSyntax
parseCocForall = do
    symbol "{\\"
    param <- option CocUnused (parseCocUnused <|> parseCocVariable)
    symbol ":"
    intype <- parseCocSyntax
    symbol "."
    body <- parseCocSyntax
    symbol "}"
    return $ CocForall param intype body

parseCocParenthesised :: Parser CocSyntax
parseCocParenthesised = do
    symbol "("
    a <- parseCocSyntax
    symbol ")"
    return a

parseCocUnused :: Parser CocSyntax
parseCocUnused = do
    symbol "_"
    return $ CocUnused

rws :: [String] -- list of reserved words
rws = ["Prop", "*", "Type", "@", "_"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> C.letterChar <*> many C.alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

parseCocVariable :: Parser CocSyntax
parseCocVariable = do
    ident <- identifier
    return $ CocVariable ident
