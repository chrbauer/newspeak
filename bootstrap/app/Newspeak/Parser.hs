{-# LANGUAGE OverloadedStrings #-}

module Newspeak.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text
import Data.Functor
import Newspeak.AST
import Control.Monad.Combinators.Expr

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
block = mathExpr <* eof

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)


pVariable :: Parser MathExpr
pVariable = MathVar <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser MathExpr
pInteger = MathInt <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser MathExpr
pTerm = choice
  [ parens mathExpr
  , pVariable
  , pInteger
  ]

mathExpr :: Parser MathExpr
mathExpr = makeExprParser pTerm operatorTable

binary :: Text -> MathOp -> Operator Parser MathExpr
binary  name op = InfixL  (MathBinExpr op <$ symbol name)

  
operatorTable :: [[Operator Parser MathExpr]]
operatorTable = [
  [binary "*" Mul]
  ]
parse :: Text -> Either (ParseErrorBundle Text Void) AST
parse code = runParser block "" code
