module Newspeak.AST where

data AST = MathExpr MathExpr | FunDecl FunDecl deriving (Show, Eq)
data MathExpr = MathBinExpr MathOp MathExpr MathExpr
              | MathInt Integer
              | MathVar String
              | MathIf BoolExpr MathExpr MathExpr
              deriving (Show, Eq)

data BoolExpr = BoolCompare MathExpr CompOp MathExpr
               | BoolLit Bool
               | BoolVar String
              deriving (Show, Eq)

data CompOp = Eq | Neq | Lt | Gt | Leq | Geq
            deriving (Show, Eq)

data FunDecl = Fun String [String] AST
         deriving (Show, Eq)

data MathOp = Add | Sub | Mul | Div
  deriving (Show, Eq)
