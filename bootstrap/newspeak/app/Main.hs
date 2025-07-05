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


-- | Split input text into blocks.  
--   Lines starting with a non-whitespace character start a new block.
splitBlocks :: String -> [String]
splitBlocks = map (unlines . reverse) . finalize . foldl step [] . lines
  where
    -- Accumulator is list of blocks, each block is list of lines (in reverse)
    step :: [[String]] -> String -> [[String]]
    step acc line
      -- skip empty lines entirely:
      | all (`elem` " \t") line = acc
      -- non-indented line → start new block:
      | head line `notElem` " \t" = [line] : acc
      -- indented line → add to current block (if any):
      | otherwise = case acc of
          (blk:rest) -> ((line : blk) : rest)
          []         -> [[line]]    -- stray indent before any header

    -- after folding, reverse blocks list and ensure it’s non-empty
    finalize :: [[String]] -> [[String]]
    finalize blks = reverse blks


    -- in Main.hs
transpile file = do
  content <- readFile file
  let blocks = splitBlocks content
  bindings <- forM blocks $ \blk ->
    case parse pBinding "<input>" blk of
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
