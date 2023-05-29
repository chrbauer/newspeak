{-# LANGUAGE OverloadedStrings #-}

module Newspeak.Compile where

import Newspeak.AST
import Language.Wasm.Structure
import Language.Wasm.Builder

compile :: AST -> Module
compile ast = genMod $ do
  export "f" $ fun i32  $ do
    ret $ i32c 42
               
               
