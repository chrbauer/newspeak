{-# LANGUAGE OverloadedStrings #-}
module Parser (pProgram, pBinding, splitBlocks) where

import           AST
import           Control.Applicative       (empty, (<|>), many, some)
import           Data.Void                 (Void)
import           Data.Map                  (Map)
import qualified           Data.List                 as L
import qualified Data.Map                  as Map
import           Text.Megaparsec           (Parsec, eof, try)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char      (alphaNumChar, letterChar, space1)
import           Text.Megaparsec.Char      (char, string)

type Parser = Parsec Void String

-- | Whitespace: spaces, tabs, newlines
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

-- | Simple SVal: integer literal or variable
pSVal :: Parser SVal
pSVal = Literal <$> integer
     <|> Var     <$> identifier

-- | Full Val: empty tuple, tagged node, or simple SVal
pVal :: Parser Val
pVal =
      (EmptyTuple   <$ symbol "()")
  <|> try (do t <- integer
              vs <- many pSVal
              return (TagN t vs))
  <|> (Tag0 <$> integer)
  <|> (SVal <$> pSVal)

-- | Constructor patterns
pCPat :: Parser CPat
pCPat =
      try (do t <- integer
              vs <- many identifier
              return (TagNPat t vs))
  <|> (Tag0Pat     <$> integer)
  <|> (LiteralPat  <$> integer)

-- | Simple expressions: unit <val> or f a b
pSExp :: Parser SExp
pSExp =
      try (Unit <$> (symbol "unit" *> pSVal))
  <|> App   <$> some pSVal

-- | Bind sequence: se ; x -> exp
pBind :: Parser Exp
pBind = do
  se <- pSExp
  _  <- symbol ";"
  v  <- identifier
  _  <- symbol "->"
  e  <- pExp
  return (Bind v se e)

-- | Case expression: case <val> of { pat -> exp, ... }
pCase :: Parser Exp
pCase = do
  _        <- symbol "case"
  val      <- pVal
  _        <- symbol "of"
  branches <- many $ do
    pat  <- pCPat
    _    <- symbol "->"
    expr <- pExp
    return (pat, expr)
  return (Case val branches)

-- | Any expression: bind, case, or simple
pExp :: Parser Exp
pExp = try pBind <|> try pCase <|> (SExp <$> pSExp)

-- | One top-level binding: f x y = exp
pBinding :: Parser (Var, Binding)
pBinding = do
  name <- identifier
  args <- many identifier
  _    <- symbol "="
  body <- pExp
  return (name, Binding args body)

-- | Full program: many bindings
pProgram :: Parser Program
pProgram =
     sc
  *> (Program . Map.fromList <$> many pBinding)
  <* sc
  <* eof


-- | Split input text into per‐binding blocks.
--   Each non‐empty, non‐indented line starts a new block.
splitBlocks :: String -> [String]
splitBlocks = map (unlines . reverse)
            . reverse
            . foldl step []
            . lines
  where
    step :: [[String]] -> String -> [[String]]
    step acc line
      -- skip completely empty or whitespace‐only lines
      | all (`elem` (" \t" :: String)) line = acc
      -- non‐indented line: start a new block
      | head line `notElem` (" \t" :: String) = [line] : acc
      -- indented line: append to the current block
      | otherwise = case acc of
          (blk:rest) -> (line : blk) : rest
          []         -> [[line]]

  
