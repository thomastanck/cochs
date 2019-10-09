module CocParser(parseCocSyntax, parseCocProgram) where

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
    first <- parseCocSyntaxOne
    (try $ parseCocSyntaxApplyRest first)
        <|> (try $ parseCocSyntaxArrowForallRest first)
        <|> (return first)
    -- (try parseCocSyntaxApply)
    -- <|> (try parseCocSyntaxArrowForall)
    -- <|> parseCocSyntaxOne

parseCocProgram :: Parser ([CocImport], [CocDefinition])
parseCocProgram = do
    sc
    imports <- many parseCocImport
    definitions <- many parseCocDefinition
    eof
    return (imports,definitions)

parseCocSyntaxOne :: Parser CocSyntax
parseCocSyntaxOne =
    parseCocSyntaxProp
    <|> parseCocSyntaxType
    <|> parseCocSyntaxLambda
    <|> parseCocSyntaxForall
    <|> parseCocSyntaxParenthesised
    <|> parseCocSyntaxVariable
    <|> parseCocSyntaxHole

parseCocSyntaxProp :: Parser CocSyntax
parseCocSyntaxProp = do
    symbol "Prop" <|> symbol "*"
    return CocSyntaxProp

parseCocSyntaxType :: Parser CocSyntax
parseCocSyntaxType = do
    symbol "Type" <|> symbol "@"
    return CocSyntaxType

parseCocSyntaxApply :: Parser CocSyntax
parseCocSyntaxApply = do
    firstExpr <- parseCocSyntaxOne
    exprs <- some parseCocSyntaxOne
    return $ foldr (\a b->CocSyntaxApply b a) firstExpr (reverse exprs)

parseCocSyntaxApplyRest :: CocSyntax -> Parser CocSyntax
parseCocSyntaxApplyRest firstExpr = do
    exprs <- some parseCocSyntaxOne
    return $ foldr (\a b->CocSyntaxApply b a) firstExpr (reverse exprs)

parseCocSyntaxLambda :: Parser CocSyntax
parseCocSyntaxLambda = do
    symbol "(\\"
    param <- option CocSyntaxUnused (parseCocSyntaxUnused <|> parseCocSyntaxVariable)
    symbol ":"
    intype <- parseCocSyntax
    symbol "."
    body <- parseCocSyntax
    symbol ")"
    return $ CocSyntaxLambda param intype body

parseCocSyntaxForall :: Parser CocSyntax
parseCocSyntaxForall = do
    symbol "{\\"
    param <- option CocSyntaxUnused (parseCocSyntaxUnused <|> parseCocSyntaxVariable)
    symbol ":"
    intype <- parseCocSyntax
    symbol "."
    body <- parseCocSyntax
    symbol "}"
    return $ CocSyntaxForall param intype body

parseCocSyntaxArrowForall :: Parser CocSyntax
parseCocSyntaxArrowForall = do
    in1 <- parseCocSyntaxOne
    in2 <- optional (symbol ":" >> parseCocSyntaxOne)
    symbol "->"
    outtype <- parseCocSyntax
    case in2 of
        Just intype ->
            return $ CocSyntaxForall in1 intype outtype
        Nothing ->
            return $ CocSyntaxForall CocSyntaxUnused in1 outtype

parseCocSyntaxArrowForallRest :: CocSyntax -> Parser CocSyntax
parseCocSyntaxArrowForallRest in1 = do
    in2 <- optional (symbol ":" >> parseCocSyntaxOne)
    symbol "->"
    outtype <- parseCocSyntax
    case in2 of
        Just intype ->
            return $ CocSyntaxForall in1 intype outtype
        Nothing ->
            return $ CocSyntaxForall CocSyntaxUnused in1 outtype

parseCocSyntaxParenthesised :: Parser CocSyntax
parseCocSyntaxParenthesised = do
    symbol "("
    a <- parseCocSyntax
    symbol ")"
    return a

parseCocSyntaxUnused :: Parser CocSyntax
parseCocSyntaxUnused = do
    symbol "_"
    return $ CocSyntaxUnused

rws :: [String] -- list of reserved words
rws = ["Prop", "*", "Type", "@", "_", "define", "=", ";", "->", "import", "as"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_' <|> C.char '\"')
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

hole_identifier :: Parser String
hole_identifier = (lexeme . try) (p >>= check)
  where
    -- p       = (:) <$> (C.char '?') <*> ((:) <$> C.letterChar <*> many C.alphaNumChar)
    p       = (:) <$> (C.char '?') <*> ((:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_' <|> C.char '\"'))
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an hole identifier"
                else return x

parseCocSyntaxVariable :: Parser CocSyntax
parseCocSyntaxVariable = do
    ident <- identifier
    return $ CocSyntaxVariable ident

parseCocSyntaxHole :: Parser CocSyntax
parseCocSyntaxHole = do
    ident <- hole_identifier
    return $ CocSyntaxHole ident

parseCocDefinition :: Parser CocDefinition
parseCocDefinition = do
    symbol "define"
    defname <- identifier
    symbol "="
    expr <- parseCocSyntax
    symbol ";"
    return $ CocDefinition defname expr

stringLiteral :: Parser String
stringLiteral = (lexeme . try) $ C.char '\"' *> manyTill L.charLiteral (C.char '\"')

parseCocImport :: Parser CocImport
parseCocImport = do
    symbol "import"
    packagename <- stringLiteral
    symbol "{"
    defmap <- many $ do
        defname <- identifier
        mapto <- option defname (symbol "as" >> identifier)
        return (defname,mapto)
    symbol "}"
    return $ CocImport packagename defmap
