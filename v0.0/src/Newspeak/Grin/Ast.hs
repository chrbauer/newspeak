module Newspeak.Grin.Ast where

type Fun = String
type Var = String

data GExpr
  = GLit Int
  | GVar Var
  | GAppl Fun [GExpr]         
  deriving (Eq, Show)

data Value
  = VInt Int
  | VUnit
  deriving (Eq, Show)
