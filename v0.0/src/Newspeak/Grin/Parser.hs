-- src/Newspeak/Grin/Parser.hs
module Newspeak.Grin.Parser (parseExpr) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (Parsec, runParser, errorBundlePretty, empty, eof, (<?>))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Newspeak.Grin.Ast

type P = Parsec Void String

sc :: P ()
sc = L.space space1 empty empty
lexeme :: P a -> P a
lexeme = L.lexeme sc
symbol :: String -> P String
symbol = L.symbol sc

lit :: P GExpr
lit = GLit <$> lexeme L.decimal <?> "integer"

term :: P GExpr
term = lit

table :: [[Operator P GExpr]]
table =
  [ [ InfixL (bin "*" "*") ]
  , [ InfixL (bin "+" "+") ]
  ]
  where
    bin tok f = GAppl f .: (\a b -> [a,b]) <$ symbol tok
    (.:) g h x y = g (h x y)

gexpr :: P GExpr
gexpr = makeExprParser term table <?> "expression"

parseExpr :: String -> Either String GExpr
parseExpr = first errorBundlePretty . runParser (sc *> gexpr <* eof) "<input>"
