module Newspeak.Grin.Ast where

data GExpr
  = GLit Int
  | GAdd GExpr GExpr
  deriving (Eq, Show)
