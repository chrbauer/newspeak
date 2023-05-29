module Newspeak.AST where

type AST = MathExpr
data MathExpr = MathExpr MathOp MathExpr MathExpr
              | MathVal Int
              deriving (Show, Eq)

data MathOp = Add | Sub | Mul | Div
  deriving (Show, Eq)
