module Newspeak.AST where

type AST = MathExpr
data MathExpr = MathBinExpr MathOp MathExpr MathExpr
              | MathInt Integer
              | MathVar String
              deriving (Show, Eq)

data MathOp = Add | Sub | Mul | Div
  deriving (Show, Eq)
