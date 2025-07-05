{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser String
keyword kw = lexeme (string kw)

pProgram :: Parser Program
pProgram = do
  _ <- keyword "program"
  e <- pExpr
  return (Program e)

pExpr :: Parser Expr
pExpr = do
  _ <- keyword "return"
  n <- lexeme L.decimal
  return (Return n)
