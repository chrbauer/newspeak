module Language.Newspeak.AST where

import Data.Text (Text)


data AST = Expr Expr | Decl Decl deriving (Show, Eq)
data Expr = ExprBinOp BinOp Expr Expr
              | ExprLit Integer
              | ExprVar String
              | ExprIf BoolExpr Expr Expr
              | ExprApply String [Expr]
              | ExprLet [Decl] Expr
              deriving (Show, Eq)

data BoolExpr = BoolCompare Expr CompOp Expr
               | BoolLit Bool
               | BoolVar String
              deriving (Show, Eq)

data CompOp = Eq | Neq | Lt | Gt | Leq | Geq
            deriving (Show, Eq)

data Decl = Fun String [String] Expr
            | DataType Name [Constructor]
         deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div
  deriving (Show, Eq)

type Program = [AST]
type Name = String

data Constructor = Constructor Name Tag [Name] deriving (Show, Eq)
data DataCtors = Map Name [Constructor] deriving (Show, Eq)
type Tag = Int
type Arity = Int
  
data Module =
  Module { moduleName :: Name
         , moduleImports :: [Name]
         , moduleExports :: [Name]
         , moduleDecls :: [Decl]
         }
  deriving (Show, Eq)
