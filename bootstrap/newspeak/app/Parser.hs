{-# LANGUAGE OverloadedStrings #-}
module Parser (pProgram, pBinding) where

import           AST
import           Control.Applicative       (empty, (<|>), many, some)
import           Data.Void                 (Void)
import qualified Data.Map                  as Map
import           Text.Megaparsec           (Parsec, eof, try)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char      (alphaNumChar, letterChar, space1)
import           Text.Megaparsec.Char (char, eol)
import           Control.Monad        (void)

type Parser = Parsec Void String

-- 1) Consume all horizontal *and* vertical whitespace
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser Var
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

integer :: Parser Int
integer = lexeme L.decimal

-- 2) Simple values
pSVal :: Parser SVal
pSVal = Literal <$> integer
     <|> Var     <$> identifier

-- 3) “unit v” or “f a b”
pSExp :: Parser SExp
pSExp =
      try (Unit <$> (symbol "unit" *> pSVal))
  <|> App   <$> some pSVal

-- 4) Sequencing: “se ; v -> exp” folds right into Bind
pExp :: Parser Exp
pExp = try pBind <|> (SExp <$> pSExp)
 where
  pBind = do
    se <- pSExp
    _  <- symbol ";"    -- sequence
    v  <- identifier    -- binder name
    _  <- symbol "->"   -- arrow
    e  <- pExp          -- rest of the expression
    return (Bind v se e)

-- 5) Top-level binding: “f x y = exp”
pBinding :: Parser (Var, Binding)
pBinding = do
  name <- identifier
  args <- many identifier
  _    <- symbol "="
  body <- pExp
  return (name, Binding args body)

-- 6) Program = many bindings until EOF
pProgram :: Parser Program
pProgram =
     sc
  *> (Program . Map.fromList <$> many pBinding)
  <* sc
  <* eof
