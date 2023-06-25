module Language.Newspeak.AST where

import Data.Text (Text)


data AST = Expr Expr | FunDecl FunDecl deriving (Show, Eq)
data Expr = ExprBinOp BinOp Expr Expr
              | ExprLit Integer
              | ExprVar String
              | ExprIf BoolExpr Expr Expr
              | ExprApply String [Expr]
              | ExprLet [FunDecl] Expr
              deriving (Show, Eq)

data BoolExpr = BoolCompare Expr CompOp Expr
               | BoolLit Bool
               | BoolVar String
              deriving (Show, Eq)

data CompOp = Eq | Neq | Lt | Gt | Leq | Geq
            deriving (Show, Eq)

data FunDecl = Fun String [String] Expr 
         deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div
  deriving (Show, Eq)

type Program = [AST]
type Name = String

data Module =
  Module { moduleName :: Name
         , moduleImports :: [Name]
         , moduleExports :: [Name]
         , moduleDecls :: [FunDecl]
         }
  deriving (Show, Eq)
