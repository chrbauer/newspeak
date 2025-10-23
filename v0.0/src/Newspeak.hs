module Newspeak (evalExpr, compileToGrin, runGrin) where

import Newspeak.Grin.Ast
import Newspeak.Grin.Parser (parseExpr)

compileToGrin :: String -> Either String GExpr
compileToGrin = parseExpr

runGrin :: GExpr -> Int
runGrin (GLit n)            = n
runGrin (GAppl "+" [a,b])   = runGrin a + runGrin b
runGrin (GAppl "*" [a,b])   = runGrin a * runGrin b
runGrin (GAppl f args)      = error ("unknown function: " ++ show f ++ "/" ++ show (length args))

evalExpr :: String -> Int
evalExpr src =
  case compileToGrin src of
    Right ge -> runGrin ge
    Left  e  -> error e
