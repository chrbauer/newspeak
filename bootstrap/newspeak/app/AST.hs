module AST where

import qualified Data.Map as Map

type Var = String

data Program = Program (Map.Map Var Binding)
  deriving (Show, Eq)

data Binding = Binding [Var] Exp
  deriving (Show, Eq)

data Exp = SExp SExp
  deriving (Show, Eq)

data SExp =
   Unit Val
  | App [SVal]
  deriving (Show, Eq)

data Val = SVal SVal
  deriving (Show, Eq)

data SVal =
    Literal Literal
  | Var Var
  deriving (Show, Eq)

type Literal = Int
