
module Codegen (emitJS) where

import AST
import qualified Data.Map as Map
import Data.List (intersperse)

emitJS :: Program -> String
emitJS (Program bindings) =
  Map.foldMapWithKey emitBinding bindings
  ++ "\nconsole.log(main());\n"

emitBinding :: Var -> Binding -> String
emitBinding name (Binding args exp) =
  "function " ++ name ++ "(" ++ emitArgs args ++ ") {\n  return " ++ emitExp exp ++ ";\n}\n\n"

emitArgs :: [Var] -> String
emitArgs = concat . intersperse ", "

emitExp :: Exp -> String
emitExp (SExp sexp) = emitSExp sexp

emitSExp :: SExp -> String
emitSExp (Unit val) = emitVal val
emitSExp (App svals) = emitApp svals

emitVal :: Val -> String
emitVal (SVal sval) = emitSVal sval

emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var v) = v

emitApp :: [SVal] -> String
emitApp (f:args) = emitSVal f ++ "(" ++ emitArgList args ++ ")"
emitApp [] = error "Empty application"

emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal
