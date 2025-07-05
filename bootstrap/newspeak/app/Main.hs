module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Parser
import Codegen
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Either (either)

import qualified Data.Map as Map
import Control.Monad (forM)
import Data.List (unlines)
import Data.Char (isSpace)
import AST




    -- in Main.hs
transpile file = do
  content <- readFile file
  let blocks = splitBlocks content
  bindings <- forM blocks $ \blk ->
    case parse pBinding file blk of
      Left err -> die (errorBundlePretty err)
      Right b  -> return b
  let prog = Program (Map.fromList bindings)
  putStrLn (emitJS prog)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> transpile inputFile
    _ -> die "Usage: grin2js <input.grin>"
