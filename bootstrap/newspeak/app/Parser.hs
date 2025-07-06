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
import           Text.Megaparsec.Char      (alphaNumChar, letterChar, space1, upperChar, lowerChar)
import           Text.Megaparsec.Char      (char, string)
import           Data.Char (isUpper, isLower)
import qualified Data.Set as Set

type Parser = Parsec Void String

reserved :: Set.Set String
reserved = Set.fromList ["case", "of"]

-- | Whitespace: spaces, tabs, newlines
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


-- identifier that rejects reserved words
identifier :: Parser Var
identifier = (lexeme . try) $ do
  ident <- (:) <$> letterChar <*> many alphaNumChar
  if ident `Set.member` reserved
     then fail $ "keyword " ++ show ident ++ " cannot be an identifier"
     else return ident

-- | Upper‐case identifier (constructor tag)
upperIdent :: Parser String
upperIdent = lexeme ((:) <$> upperChar <*> many alphaNumChar)

-- | Lower‐case identifier (variable tag)
lowerIdent :: Parser String
lowerIdent = lexeme ((:) <$> lowerChar <*> many alphaNumChar)

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
  <|> try (do t <- identifier
              vs <- many pSVal
              return (TagN t vs))
  <|> (Tag0 <$> identifier)
  <|> (SVal <$> pSVal)

pCPat :: Parser CPat
pCPat =
      -- integer literal pattern
      LiteralPat <$> integer
  <|> try (do
        -- lower‐case “constructor” pattern
        v  <- lowerIdent
        vs <- many identifier
        return (TagVarPat v vs))
  <|> try (do
        -- upper‐case constructor with fields
        t  <- upperIdent
        vs <- many identifier
        return (TagNPat t vs))
  <|> (Tag0Pat <$> upperIdent)  

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
pExp = pCase <|> try pBind <|> (SExp <$> pSExp)

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

  
