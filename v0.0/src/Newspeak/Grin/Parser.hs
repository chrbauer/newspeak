module Newspeak.Grin.Parser (parseExpr) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Newspeak.Grin.Ast

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal <?> "integer"

gexpr :: Parser GExpr
gexpr = do
  a <- integer
  _ <- symbol "+"
  b <- integer
  pure (GAdd (GLit a) (GLit b))

parseExpr :: String -> Either String GExpr
parseExpr = first errorBundlePretty . runParser (sc *> gexpr <* eof) "<input>"
