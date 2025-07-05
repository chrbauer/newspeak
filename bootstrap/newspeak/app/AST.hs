module AST where

import qualified Data.Map as Map

type Var = String

data Program = Program (Map.Map Var Binding)
  deriving (Show, Eq)

data Binding = Binding [Var] Exp
  deriving (Show, Eq)

-- Expressions support simple expressions and sequencing
-- SExp: Unit wraps a value, App for function call
data Exp
  = SExp SExp
  | Bind Var SExp Exp
  deriving (Show, Eq)

data SExp
  = Unit SVal        -- unit <value>
  | App [SVal]       -- application: first element is function
  deriving (Show, Eq)

data SVal
  = Literal Int      -- integer literal
  | Var Var          -- variable
  deriving (Show, Eq)

