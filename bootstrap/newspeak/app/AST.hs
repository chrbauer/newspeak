module AST where

import qualified Data.Map as Map

type Var = String
type Literal = Int

data Program = Program (Map.Map Var Binding)
  deriving (Show, Eq)

data Binding = Binding [Var] Exp
  deriving (Show, Eq)

-- Expressions support simple expressions and sequencing
-- SExp: Unit wraps a value, App for function call
data Exp
  = SExp SExp
  | Bind Var SExp Exp
  | Case Val [(CPat, Exp)]
  deriving (Show, Eq)

type Tag = String



data CPat =
    TagNPat Tag [Var]
  | TagVarPat Var [Var]
  | Tag0Pat Tag
  | LiteralPat Literal
   deriving (Show, Eq)
  

data SExp
  = Unit SVal        -- unit <value>
  | App [SVal]       -- application: first element is function
  | Store  SVal               -- store <value>
  | Fetch  Var (Maybe Int)    -- fetch p [n]
  | Update Var SVal           -- update p <value>
  | Exp Exp
  deriving (Show, Eq)

data SVal
  = Literal Literal       -- integer literal
  | Var Var          -- variable
  deriving (Show, Eq)

data Val =
    TagN Tag [SVal]
  | Tag0 Tag
  | EmptyTuple
  | SVal SVal 

  deriving (Show, Eq)
