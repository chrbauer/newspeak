module Newspeak.Repl (replStep, replPure, runReplIO) where

import System.IO (hFlush, stdout, isEOF)
import Newspeak (compileToGrin, runGrin)

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

runReplIO :: IO ()
runReplIO = loop where
  loop = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    if eof then pure () else do
      line <- getLine
      case replStep line of
        Nothing  -> pure ()
        Just out -> putStrLn out >> loop
