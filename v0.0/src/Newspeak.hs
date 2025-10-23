module Newspeak (evalExpr, repl) where

evalExpr :: String -> Int
evalExpr "2+2" = 4
evalExpr _     = error "unimplemented"

repl :: IO ()
repl = putStrLn "Welcome to Newspeak REPL!"