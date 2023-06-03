{-# LANGUAGE OverloadedStrings #-}

module Main where

import Newspeak.AST
import Newspeak.Compile
import Newspeak.Parser
import Language.Wasm.Binary
import Language.Wasm.Interpreter
import Language.Wasm.Validate
import qualified Data.ByteString.Lazy as BS
import Text.Megaparsec.Error
import Data.Text (Text)
import qualified Data.Text as T

import System.Console.Haskeline

import Control.Monad.IO.Class

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                liftIO $ run (T.pack input)
                                loop

run :: Text -> IO ()
run line = do
  case parse line of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> do
      let m = compile ast
      let Right vm = validate m
      let bin = dumpModuleLazy m
      BS.writeFile "m.wasm" bin
      (Right im, store') <- instantiate emptyStore emptyImports vm
      r <- invokeExport store' im "f" []  
      print r
      putStrLn "Done"
  
