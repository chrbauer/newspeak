module Codegen where

import AST

emitJS :: Program -> String
emitJS (Program expr) =
  "function main() {\n  " ++ emitExpr expr ++ "\n}"

emitExpr :: Expr -> String
emitExpr (Return n) = "return " ++ show n ++ ";"
