module Newspeak.Repl (runReplIO, replStep, replPure) where

import System.Console.Haskeline
import qualified Data.Map.Strict as M
import Newspeak.Module (emptyModule, Module)
import Newspeak.Grin.Ast 
import Newspeak.Eval (evalTop)

replStep :: Module -> String -> (Module, Maybe String)
replStep m s
  | s == ":q" || s == ":quit" = (m, Nothing)
  | otherwise =
      case evalTop m s of
        Left err -> (m, Just ("error: " ++ err))
        Right (m', val) ->
          case val of
            VInt 0 | isDef s -> (m', Nothing)
            VInt n           -> (m', Just (show n))
            _                -> (m', Nothing)
  where
    -- crude heuristic: a definition contains '=' before any operator
    isDef str = '=' `elem` str && not (any (`elem` str) "+-*//")

replPure :: [String] -> [String]
replPure = go emptyModule []
  where
    go _ acc [] = reverse acc
    go m acc (x:xs) =
      let (m', out) = replStep m x
      in case out of
           Nothing  -> go m' acc xs
           Just str -> go m' (str:acc) xs

-- interactive REPL using Haskeline
runReplIO :: IO ()
runReplIO = runInputT defaultSettings (loop emptyModule)
  where
    loop m = do
      minput <- getInputLine "? "
      case minput of
        Nothing      -> pure ()
        Just ":q"    -> pure ()
        Just ":quit" -> pure ()
        Just line    ->
          let (m', out) = replStep m line
          in do
            maybe (pure ()) outputStrLn out
            loop m'