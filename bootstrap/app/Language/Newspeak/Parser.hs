{-# LANGUAGE OverloadedStrings #-}

module Language.Newspeak.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text
import Data.Functor
import Language.Newspeak.AST
import Control.Monad.Combinators.Expr
import Data.Text as T


type Parser = Parsec Void Text



noWS :: Parser ()
noWS = return ()

sc :: Parser ()
sc = L.space
  space1                         
  (L.skipLineComment "#")       
  (L.skipBlockCommentNested "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

block :: Parsec Void Text AST
block =  sc *> ((FunDecl <$> try funDecl) <|> (MathExpr <$> mathExpr))  

identifier :: Parser String
identifier = lexeme  ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

funDecl :: Parser FunDecl
funDecl = do
  name <- identifier
  args <- sepBy identifier sc
  symbol "="
  body <- mathExpr
  return $ Fun name args body

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)


pVariable :: Parser MathExpr
pVariable = MathVar <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  
pInteger :: Parser MathExpr
pInteger = MathInt <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ifThenElse :: Parser MathExpr
ifThenElse = do
  pKeyword "if"
  cond <- boolExpr
  pKeyword "then"
  thenExpr <- mathExpr
  pKeyword "else"
  elseExpr <- mathExpr
  return $ MathIf cond thenExpr elseExpr


boolExpr :: Parser BoolExpr
boolExpr =  try boolCompare <|> boolLit <|> boolVar

boolLit :: Parser BoolExpr
boolLit = BoolLit <$> (pKeyword "True" $> True <|> pKeyword "False" $> False)

boolVar :: Parser BoolExpr
boolVar = BoolVar <$> identifier

boolCompare :: Parser BoolExpr
boolCompare = do
  left <- mathExpr
  op <- pKeyword "==" $> Eq <|> pKeyword "<=" $> Leq <|> pKeyword ">=" $> Geq
  right <- mathExpr
  return $ BoolCompare left op right

pTerm :: Parser MathExpr
pTerm = choice
  [ parens mathExpr
  , ifThenElse
  , funCall
  , pVariable
  , pInteger
  ]

mathExpr :: Parser MathExpr
mathExpr = makeExprParser pTerm operatorTable

funCall :: Parser MathExpr
funCall = try $ do
  name <- identifier
  args <- sepBy mathExpr sc
  return $ MathFunCall name args

binary :: Text -> MathOp -> Operator Parser MathExpr
binary  name op = InfixL  (MathBinExpr op <$ symbol name)

  
operatorTable :: [[Operator Parser MathExpr]]
operatorTable = [
  [binary "*" Mul, binary "/" Div],
  [binary "+" Add, binary "-" Sub]    
  ]
parse :: Text -> Either (ParseErrorBundle Text Void) [AST]
parse code = runParser (sepBy block (char ';') <* eof) "" code
