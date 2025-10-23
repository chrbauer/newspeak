module Newspeak.Grin.Ast where

type Fun = String

data GExpr
  = GLit Int
  | GAppl Fun [GExpr]    -- e.g. "+" [e1,e2], "*" [e1,e2]
  deriving (Eq, Show)
