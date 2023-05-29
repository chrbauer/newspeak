{-# LANGUAGE OverloadedStrings #-}

module Newspeak.Compile where

import Newspeak.AST
import Language.Wasm.Structure
import Language.Wasm.Builder
import Newspeak.AST (MathExpr(MathVal))

compile :: AST -> Module
compile ast = genMod $ do
  export "f" $ fun i32  $ do
    case ast of
      MathVal x -> ret $ i32c x
      _ -> ret $ i32c 0
               
               
