{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Newspeak.AST
-- import Language.Newspeak.Compile
import Language.Newspeak.GenCore
import Language.Newspeak.Parser
import Language.Wasm.Binary
import Language.Wasm.Interpreter
import Language.Wasm.Validate
import qualified Data.ByteString.Lazy as BS
import Text.Megaparsec.Error
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Console.Haskeline

import Control.Monad.IO.Class
import Language.Newspeak.Core (pprint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interactive
    [file] -> do
      contents <- T.readFile file
      run contents
    _ -> putStrLn "Usage: newspeak [file]"
  

interactive :: IO ()
interactive = runInputT defaultSettings (loop [])
   where
       loop :: [String] -> InputT IO ()
       loop p = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just "" ->  do
                 let input = unlines $ reverse p
                 outputStrLn $ "Input was: " ++ input               
                 liftIO $ run (T.pack input)
                 loop []
               Just input -> loop (input:p)

run :: Text -> IO ()
run line = do
  case parse line of
    Left err -> putStrLn $ errorBundlePretty err
    Right m -> do
      putStrLn $ show m
      --let core = genCore m
      --putStrLn $ show $ pprint core
      -- let m = compile ast
      -- let Right vm = validate m
      -- let bin = dumpModuleLazy m
      -- BS.writeFile "m.wasm" bin
      -- (Right im, store') <- instantiate emptyStore emptyImports vm
      -- r <- invokeExport store' im "main" []  
      -- print r
      putStrLn "Done"
  
