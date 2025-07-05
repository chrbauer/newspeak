-- Codegen.hs
module Codegen (emitJS) where

import           AST
import           Data.List            (intersperse)
import qualified Data.Map             as Map

emitJS :: Program -> String
emitJS (Program bindings) =
     Map.foldMapWithKey emitBinding bindings
  ++ "\nconsole.log(main());\n"

emitBinding :: Var -> Binding -> String
emitBinding name (Binding args exp) =
  "function " ++ name ++ "(" ++ emitArgs args ++ ") {\n"
  ++ unlines (map ("  " ++) (emitLines exp))
  ++ "}\n\n"

emitArgs :: [Var] -> String
emitArgs = concat . intersperse ", "

-- flatten binds into JS statements
emitLines :: Exp -> [String]
emitLines (Bind v se rest) =
  ("const " ++ v ++ " = " ++ emitSExp se ++ ";")
  : emitLines rest
emitLines (SExp se) =
  ["return " ++ emitSExp se ++ ";"]

emitSExp :: SExp -> String
emitSExp (Unit sval)   = emitSVal sval
emitSExp (App (f:xs))  = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (App [])      = error "emitSExp: empty application"

emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var      v) = v

emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal
