{-# LANGUAGE OverloadedStrings #-}

module Newspeak.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text

import Newspeak.AST

type Parser = Parsec Void Text


mathExpr  :: Parser  AST
mathExpr =  MathVal <$> L.decimal



parse :: Text -> Either (ParseErrorBundle Text Void) AST
parse code = runParser mathExpr "" code
