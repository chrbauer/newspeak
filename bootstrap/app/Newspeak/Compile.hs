{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Newspeak.Compile where

import Newspeak.AST
import Language.Wasm.Structure
import Language.Wasm.Builder
import Newspeak.AST 
import Data.Proxy


compile :: AST -> Module
compile ast = genMod $ export "f" $ fun i32  $ ret (genExpr ast)
  where
        genExpr :: AST -> GenFun (Proxy I32)
        genExpr ast =
          case ast of
            MathInt x -> i32c x
            MathBinExpr op x y -> do
               let x' = genExpr x
                   y' = genExpr y
               case op of
                 Add -> x' `add` y'
                 Sub -> x' `sub` y'
                 Mul -> x' `mul` y'
                 Div -> x' `div_u` y'
               
               
