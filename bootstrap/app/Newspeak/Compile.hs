{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Newspeak.Compile where

import Control.Monad
import Control.Monad.State.Strict
import Newspeak.AST
import Language.Wasm.Structure
import Language.Wasm.Builder
import Newspeak.AST 
import Data.Proxy
import Newspeak.AST (AST(MathExpr, FunDecl))
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M


data Scope = Scope {
  scopeParams :: M.Map String (Loc I32),
  scopeLocals :: M.Map String (Loc I32),
  scopeFuncs :: M.Map String (Loc I32)
}

emptyScope = Scope M.empty M.empty M.empty

compile :: AST -> Module
compile ast = genMod $ compileAST ast
  where
        compileAST (MathExpr expr) =  export "f" $ fun i32  $ ret $ genExpr emptyScope expr
        compileAST (FunDecl fn) =  genFn fn
        compileCond _ (BoolLit True) =  i32c 1
        compileCond _ (BoolLit False) =  i32c 0
        compileCond scope (BoolCompare  left op right) =  do
          let left' = genExpr scope left
              right' = genExpr scope right
          case op of
            Eq -> eq left' right'
            Leq -> le_s left' right'
            Geq -> ge_s left' right'
        genExpr :: Scope -> MathExpr -> GenFun (Proxy I32)
        genExpr scope expr =
          case expr of
            MathInt x -> i32c x
            MathIf cond e1 e2 -> 
                if' (Inline $ Just I32) (compileCond scope cond) (genExpr scope e1) (genExpr scope e2)
            MathBinExpr op x y -> do
                let x' = genExpr scope x
                    y' = genExpr scope y
                case op of
                  Add -> x' `add` y'
                  Sub -> x' `sub` y'
                  Mul -> x' `mul` y'
                  Div -> x' `div_u` y'
        genFn (Fun name args body) =  do
          export (TL.pack name) $ fun i32 $ do
            args' <-  mapM (\ a ->   (a,) <$> param i32) args        -- args' <- mapM (\ n -> (n,) <$> param i32) args          
            let  scope = emptyScope { scopeParams = M.fromList args' }
                 body' = genExpr scope body
            ret body'
               
               
