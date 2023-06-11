module Language.Newspeak.GenCore where

import qualified Language.Newspeak.Core as Core
import qualified Language.Newspeak.AST as AST

genCore :: AST.Program -> Core.Program AST.Name
genCore = map genDef

genDef :: AST.AST -> Core.ScDefn AST.Name
genDef (AST.FunDecl (AST.Fun name args body)) = (name, args, genExpr body)


genExpr :: AST.MathExpr -> Core.Expr a
genExpr (AST.MathInt n) = Core.ENum (fromIntegral n)
genExpr (AST.MathVar v) = Core.EVar v
genExpr (AST.MathFunCall f args) = foldl (\ r a -> Core.EAp r  (genExpr a)) (Core.EVar f) args
