module Newspeak (evalExpr, compileToGrin, runGrin) where

import Newspeak.Grin.Ast
import Newspeak.Grin.Parser (parseExpr)

compileToGrin :: String -> Either String GExpr
compileToGrin = parseExpr

runGrin :: GExpr -> Int
runGrin (GLit n)        = n
runGrin (GAdd (GLit a) (GLit b)) = a + b
runGrin _ = error "runtime error: only A+B of literals supported"

-- Kept Int API for now. Errors if parse fails.
evalExpr :: String -> Int
evalExpr src =
  case compileToGrin src of
    Right ge -> runGrin ge
    Left err  -> error $ "unimplemented: " ++ err 
