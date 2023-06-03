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

main :: IO ()
main = do
  case parse "if 6 >= 7 then 1 else 2" of
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
  
