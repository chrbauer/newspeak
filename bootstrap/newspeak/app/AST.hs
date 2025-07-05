module AST where

data Program = Program Expr deriving (Show)
data Expr = Return Int deriving (Show)
