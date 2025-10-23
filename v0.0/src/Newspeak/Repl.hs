module Newspeak.Repl (replStep, replPure, runReplIO) where

import System.IO (hFlush, stdout, isEOF)
import Newspeak (compileToGrin, runGrin)

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline

runReplIO :: IO ()
runReplIO = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing     -> return ()  -- Ctrl-D
        Just ":q"   -> return ()
        Just ":quit" -> return ()
        Just line   ->
          case compileToGrin line of
            Left err  -> outputStrLn ("error: " ++ err) >> loop
            Right ast -> outputStrLn (show (runGrin ast)) >> loop

replStep :: String -> Maybe String
replStep s
  | s == ":q" || s == ":quit" = Nothing
  | otherwise = Just $ either (\e -> "error: " ++ e) (show . runGrin) (compileToGrin s)

replPure :: [String] -> [String]
replPure = go [] where
  go acc [] = reverse acc
  go acc (x:xs) =
    case replStep x of
      Nothing  -> reverse acc
      Just out -> go (out:acc) xs
