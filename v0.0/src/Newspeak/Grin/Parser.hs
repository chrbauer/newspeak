module Newspeak.Grin.Parser (parseExpr) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Newspeak.Grin.Ast

type Parser = Parsec Void String

-- whitespace
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser GExpr
integer = GLit <$> lexeme L.decimal <?> "integer"

gexpr :: Parser GExpr
gexpr = makeExprParser integer table <?> "expression"

table :: [[Operator Parser GExpr]]
table =
  [ [ InfixL (GAdd <$ symbol "+") ]
  ]

parseExpr :: String -> Either String GExpr
parseExpr = first errorBundlePretty . runParser (sc *> gexpr <* eof) "<input>"
