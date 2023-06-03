{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Newspeak.Compile where

import Newspeak.AST
import Language.Wasm.Structure
import Language.Wasm.Builder
import Newspeak.AST 
import Data.Proxy
import Newspeak.AST (AST(MathExpr, FunDecl))
import qualified Data.Text.Lazy as TL


compile :: AST -> Module
compile ast = genMod $ compileAST ast
  where
        compileAST (MathExpr expr) =  export "f" $ fun i32  $ ret $ genExpr expr
        compileAST (FunDecl fn) =  genFn fn
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
        genFn (Fun name args body) = do
          let body' = genExpr body          
          export (TL.pack name) $ fun i32 $ ret body'
               
               
