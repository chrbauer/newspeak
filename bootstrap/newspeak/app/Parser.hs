{-# LANGUAGE OverloadedStrings #-}
module Parser (pProgram, pBinding, splitBlocks) where

import           AST
import           Control.Applicative       (empty, (<|>), many, some, optional)
import           Data.Void                 (Void)
import           Data.Map                  (Map)
import qualified           Data.List                 as L
import qualified Data.Map                  as Map
import           Text.Megaparsec           (Parsec, eof, try, between, sepBy1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char      (alphaNumChar, letterChar, space1, upperChar, lowerChar)
import           Text.Megaparsec.Char      (char, string)
import           Data.Char (isUpper, isLower)
import qualified Data.Set as Set
import           Control.Monad              (when)

type Parser = Parsec Void String

reserved :: Set.Set String
reserved = Set.fromList ["case", "of", "store", "fetch", "update", "unit", "->", ";", "=", "->"]

-- | Whitespace: spaces, tabs, newlines
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pFirst :: Parser Char
pFirst = letterChar <|> char '_'

pRest :: Parser Char
pRest = alphaNumChar <|> char '_' <|> char '\''

mkIdent :: Parser Char -> Parser Char -> Parser String
mkIdent pFirst pRest = lexeme $ try $ do
  f  <- pFirst
  rs <- many pRest
  let ident = f:rs
  when (ident `Set.member` reserved) $
    fail $ "keyword " ++ show ident ++ " cannot be an identifier"
  return ident


identifier :: Parser String
identifier = mkIdent pFirst pRest

lowerIdent :: Parser String
lowerIdent = mkIdent (lowerChar <|> char '_') pRest

upperIdent :: Parser String
upperIdent = mkIdent (upperChar <|> char '_') pRest

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
    <|>    between (symbol "(") (symbol ")") pVal

  <|> try (TagN  <$> upperIdent <*> many pSVal)
  <|>      (Tag0 <$> upperIdent)

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
     Exp   <$> between (symbol "(") (symbol ")") pExp
  <|>   try (Store <$> (symbol "store" *> pVal))
  <|> try (Fetch <$> (symbol "fetch" *> identifier)
                 <*>  optional integer)
  <|> try (Update <$> (symbol "update" *> identifier)
                  <*> pVal)
  <|> try (Unit   <$> (symbol "unit"   *> pVal))
  <|> App   <$> some pSVal  

-- | Bind sequence: se ; x -> exp

pBind :: Parser Exp
pBind = do
   se  <- pSExp
   _   <- symbol ";"
   lp  <- pVal              -- parse a full value pattern, e.g. `(CInt r')` or `x`
   _   <- symbol "->"
   e   <- pExp
   return (Bind lp se e)  

-- parse a case expression
pCase :: Parser Exp
pCase = do
  _    <- symbol "case"
  val  <- pVal                    -- b'
  _    <- symbol "of"
  branches <- pBranch `sepBy1` symbol "|"
  return (Case val branches)

-- parse one branch: CTrue -> expr
pBranch :: Parser (CPat, Exp)
pBranch = do
  pat <- pCPat                    -- CTrue, CFalse, TagNPat, LiteralPat, …
  _   <- symbol "->"
  expr <- pExp
  return (pat, expr)

-- integrate into your main expression parser:
pExp :: Parser Exp
pExp =   try pBind 
     <|> (SExp <$> pSExp)
     <|> pCase                 

-- -- | Any expression: bind, case, or simple
-- pExp :: Parser Exp
-- pExp = pCase <|> try pBind <|> (SExp <$> pSExp)

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

  
