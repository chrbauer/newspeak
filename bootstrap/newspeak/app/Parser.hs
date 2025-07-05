{-# LANGUAGE OverloadedStrings #-}
module Parser (pProgram) where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Map as Map

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

integer :: Parser Int
integer = lexeme L.decimal

-- SVal: Literal or Var
pSVal :: Parser SVal
pSVal = (Literal <$> integer) <|> (Var <$> identifier)

-- Val
pVal :: Parser Val
pVal = SVal <$> pSVal

-- SExp
pSExp :: Parser SExp
pSExp =
      (symbol "unit" *> (Unit <$> pVal))
  <|> (App <$> some pSVal)

-- Exp
pExp :: Parser Exp
pExp = SExp <$> pSExp

-- Binding: f x y = exp
pBinding :: Parser (Var, Binding)
pBinding = do
  name <- identifier
  args <- many identifier
  _ <- symbol "="
  body <- pExp
  return (name, Binding args body)

-- Program: many bindings
pProgram :: Parser Program
pProgram = do
  sc
  bindings <- many (pBinding <* sc)
  return (Program (Map.fromList bindings))
