module Codegen (emitJS) where

import           AST
import           Data.List            (intersperse)
import qualified Data.Map             as Map

-- | Generate the full JS program
emitJS :: Program -> String
emitJS (Program bindings) =
     Map.foldMapWithKey emitBinding bindings
  ++ "\nconsole.log(main());\n"

-- | Emit one function’s definition
emitBinding :: Var -> Binding -> String
emitBinding name (Binding args body) =
  "function " ++ name ++ "(" ++ emitArgs args ++ ") {\n"
  ++ unlines (map ("  " ++) (emitLines body))
  ++ "}\n\n"

-- | Emit comma-separated argument list
emitArgs :: [Var] -> String
emitArgs = concat . intersperse ", "

-- | Emit a sequence of JS statements or the final return
emitLines :: Exp -> [String]
emitLines (Bind v se rest) =
  ("const " ++ v ++ " = " ++ emitSExp se ++ ";")
  : emitLines rest
emitLines (SExp se) =
  ["return " ++ emitSExp se ++ ";"]
emitLines (Case val branches) = emitCase val branches

-- | Emit a JavaScript switch for a Case expression
emitCase :: Val -> [(CPat, Exp)] -> [String]
emitCase val branches =
     ["switch (" ++ emitVal val ++ ") {"] 
  ++ concatMap emitBranch branches
  ++ ["}"]
  where
    emitBranch :: (CPat, Exp) -> [String]
    emitBranch (pat, expr) =
      let header = case pat of
            TagNPat t _  -> "case " ++ show t ++ ": {"
            Tag0Pat t    -> "case " ++ show t ++ ":"
            LiteralPat n -> "case " ++ show n ++ ":"
          body   = map ("  " ++) (emitLines expr)
          end    = case pat of
            TagNPat _ vs -> 
              -- for TagNPat we might want to extract fields; placeholders for now
              ["  // fields: " ++ show vs]
              ++ body
              ++ ["  break;", "}"]
            _ ->
              body ++ ["  break;"]
      in header : end

-- | Emit simple and compound values
emitVal :: Val -> String
emitVal (SVal sval)    = emitSVal sval
emitVal (Tag0 t)       = show t
emitVal (TagN t _)     = show t  -- ignoring fields in switch key
emitVal EmptyTuple     = "undefined"  -- or "null" as desired

-- | Emit simple expressions
emitSExp :: SExp -> String
emitSExp (Unit sval)  = emitSVal sval
emitSExp (App (f:xs)) = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (App [])     = error "emitSExp: empty application"

-- | Emit simple values or variables
emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var v)     = v

-- | Emit argument list
emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal
{-# LANGUAGE OverloadedStrings #-}
module Parser (pProgram) where

import           AST
import           Control.Applicative       (empty, (<|>), many, some)
import           Data.Void                 (Void)
import           Data.Map                  (Map)
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
