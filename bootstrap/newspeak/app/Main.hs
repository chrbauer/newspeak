module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Parser
import Codegen
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Either (either)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      content <- readFile inputFile
      case parse pProgram inputFile content of
        Left err -> die (errorBundlePretty err)
        Right prog -> putStrLn (emitJS prog)
    _ -> die "Usage: grin2js <input.grin>"
