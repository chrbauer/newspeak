module Newspeak.Grin.Parser
  ( parseExpr
  , Top(..)
  , parseTop
  ) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec -- (Parsec, runParser, errorBundlePretty, eof, try, (<|>), (<?>), many, betwee, empty)
import Text.Megaparsec.Char (space1, letterChar, alphaNumChar, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Newspeak.Grin.Ast

type P = Parsec Void String

data Top = TDef { topDef :: (Fun,[Var],GExpr) }
         | TExpr { topExpr :: GExpr }
         deriving (Eq, Show)

sc :: P ()
sc = L.space space1 empty empty
lexeme :: P a -> P a
lexeme = L.lexeme sc
symbol :: String -> P String
symbol = L.symbol sc

ident :: P String
ident = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

lit :: P GExpr
lit = GLit <$> lexeme L.decimal <?> "integer"

varP :: P GExpr
varP = GVar <$> ident

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")


term :: P GExpr
term = choice
  [ lit
  , GVar <$> ident
  , parens gexpr
  ]

table :: [[Operator P GExpr]]
table =
  [ [ InfixL (bin "*" "*")
    , InfixL (bin "//" "//")
    ]
  , [ InfixL (bin "+" "+")
    , InfixL (bin "-" "-")
    ]
  ]
  where
    bin tok f = (\a b -> GAppl f [a,b]) <$ symbol tok

gexpr :: P GExpr
gexpr = makeExprParser app table <?> "expression"
  where
    app = do
      f <- term
      args <- many term
      pure $ case f of
        GVar name | not (null args) -> GAppl name args
        _ -> f

funDef :: P Top
funDef = do
  f <- ident
  args <- many ident
  _ <- symbol "="
  body <- gexpr
  pure (TDef (f,args,body))

top :: P Top
top = try funDef <|> TExpr <$> gexpr

parseExpr :: String -> Either String GExpr
parseExpr = first errorBundlePretty . runParser (sc *> gexpr <* eof) "<input>"

parseTop :: String -> Either String Top
parseTop = first errorBundlePretty . runParser (sc *> top <* eof) "<input>"
