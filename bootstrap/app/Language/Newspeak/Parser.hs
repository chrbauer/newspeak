{-# LANGUAGE OverloadedStrings #-}

module Language.Newspeak.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Text (Text)
import Data.Functor ( ($>), void )
import Language.Newspeak.AST
import Control.Monad.Combinators.Expr
    ( Operator(InfixL), makeExprParser )
import qualified Data.Text as T
import Control.Applicative ()
import GHC.Base (inline)

type Parser = Parsec Void Text



noWS :: Parser ()
noWS = return ()

sc :: Parser ()
sc = L.space inlineWS
      lineComment
      (L.skipBlockCommentNested "(*" "*)")


scn :: Parser ()
scn = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

inlineWS :: Parser ()
inlineWS = void $ some (char ' ' <|> char '\t')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

block :: Parsec Void Text AST
block =  sc *> ((Decl <$> try pFunDecl) <|> (Expr <$> pExpr))  

identifier :: Parser String
identifier = lexeme  ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")


pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)


pVariable :: Parser Expr
pVariable = ExprVar <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  
pInteger :: Parser Expr
pInteger = ExprLit . LitInt <$> lexeme L.decimal

pLiteral :: Parser Expr
pLiteral = pInteger <|> pBoolLit

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ifThenElse :: Parser Expr
ifThenElse = do
  pKeyword "if"
  cond <- pExpr
  pKeyword "then"
  thenExpr <- pExpr
  pKeyword "else"
  elseExpr <- pExpr
  return $ ExprIf cond thenExpr elseExpr


-- boolExpr :: Parser BoolExpr
-- boolExpr =  try boolCompare <|> boolLit <|> boolVar

pBoolLit :: Parser Expr
pBoolLit = ExprLit . LitBool <$> (pKeyword "True" $> True <|> pKeyword "False" $> False)

-- boolVar :: Parser BoolExpr
-- boolVar = BoolVar <$> identifier

-- boolCompare :: Parser BoolExpr
-- boolCompare = do
--   left <- pExpr
--   op <- pKeyword "==" $> Eq <|> pKeyword "<=" $> Leq <|> pKeyword ">=" $> Geq
--   right <- pExpr
--   return $ BoolCompare left op right

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , ifThenElse
  , pLet
  , funCall
  , pVariable
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pLet :: Parser Expr
pLet = L.indentBlock scn p
  where p = do
          h <- L.lineFold scn $ \sc' -> pKeyword "let" *> sc' *> pFunDecl
          return $ L.IndentMany Nothing (expr h) pFunDecl
        expr  h env = do
          pKeyword "in"
          body <- pExpr
          return $ ExprLet (h:env) body


funCall :: Parser Expr
funCall = try $ do
  name <- identifier
  args <- sepBy pArg sc
  return $ ExprApply name args

pArg :: Parser Expr
pArg = choice [
      parens pExpr
    , pVariable
    , pInteger
    ]

binary :: Text -> BinOp -> Operator Parser Expr
binary  name op = InfixL  (ExprBinOp op <$ symbol name)

  
operatorTable :: [[Operator Parser Expr]]
operatorTable = [
  [binary "*" Mul, binary "/" Div],
  [binary "+" Add, binary "-" Sub]    
  ]



pModule :: Name -> Parser Module
pModule name = Module name [] [] <$>  (some (pTopLevel <* scn) <* eof)



pFunDecl :: Parser Decl
pFunDecl = do
  name <- identifier
  args <- sepBy identifier sc
  symbol "="
  body <- pExpr
  return $ Fun name args body


pDataDecl :: Parser Decl
pDataDecl = do
  pKeyword "type"
  name <- identifier
  symbol "="
  constructors <- sepBy1 pConstructor (symbol "|")
  return $ DataType name constructors

pConstructor :: Parser Constructor
pConstructor = do
  name <- identifier
  args <- many identifier
  return $ Constructor name 42 args
  

pTopLevel :: Parser Decl
pTopLevel = L.nonIndented scn pFunDecl

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"
      
parse :: Text -> Either (ParseErrorBundle Text Void) Module
parse code = runParser (pModule "Main" <* scn) "" code


