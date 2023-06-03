{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Newspeak.Compile where

import Newspeak.AST
import Language.Wasm.Structure
import Language.Wasm.Builder
import Newspeak.AST 
import Data.Proxy
import Newspeak.AST (AST(MathExpr))


compile :: AST -> Module
compile ast = genMod $ export "f" $ fun i32  $ ret (compileAST ast)
  where
        compileAST (MathExpr expr) =  genExpr expr
        compileCond (BoolLit True) =  i32c 1
        compileCond (BoolLit False) =  i32c 0
        compileCond (BoolCompare  left op right) =  do
          let left' = genExpr left
              right' = genExpr right
          case op of
            Eq -> eq left' right'
            Leq -> le_s left' right'
            Geq -> ge_s left' right'
        genExpr :: MathExpr -> GenFun (Proxy I32)
        genExpr expr =
          case expr of
            MathInt x -> i32c x
            MathIf cond e1 e2 ->  if' (Inline $ Just I32) (compileCond cond) (genExpr e1) (genExpr e2)
            MathBinExpr op x y -> do
               let x' = genExpr x
                   y' = genExpr y
               case op of
                 Add -> x' `add` y'
                 Sub -> x' `sub` y'
                 Mul -> x' `mul` y'
                 Div -> x' `div_u` y'
               
               
