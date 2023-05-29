{-# LANGUAGE OverloadedStrings #-}

module Main where

import Newspeak.AST
import Newspeak.Compile
import Language.Wasm.Binary
import Language.Wasm.Interpreter
import Language.Wasm.Validate
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  let m = compile (MathVal 31)
  let Right vm = validate m
  let bin = dumpModuleLazy m
  BS.writeFile "m.wasm" bin
  (Right im, store') <- instantiate emptyStore emptyImports vm
  r <- invokeExport store' im "f" []
  
  print r
  putStrLn "Done"
  
