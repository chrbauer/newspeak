module Language.Newspeak.GenCore where

import qualified Language.Newspeak.Core as Core
import qualified Language.Newspeak.AST as Newspeak

genCore :: Newspeak.Module -> Core.Program Newspeak.Name
genCore m = map genDef $ Newspeak.moduleDecls m

genDef :: Newspeak.FunDecl -> Core.ScDefn Newspeak.Name
genDef (Newspeak.Fun name args body) = (name, args, genExpr body)


genExpr :: Newspeak.Expr -> Core.Expr Core.Name
genExpr (Newspeak.ExprLit n) = Core.ENum (fromIntegral n)
genExpr (Newspeak.ExprVar v) = Core.EVar v
genExpr (Newspeak.ExprApply f args) = foldl (\ r a -> Core.EAp r  (genExpr a)) (Core.EVar f) args
genExpr (Newspeak.ExprLet binds args) = Core.ELet True (map genBindings binds) (genExpr args)
genExpr (Newspeak.ExprBinOp op e1 e2) = Core.EAp (Core.EAp (Core.EPrim (primOp op)) (genExpr e1)) (genExpr e2)

-- genBindings :: Newspeak.FunDecl -> (a, Core.Expr a)
genBindings (Newspeak.Fun name [] body) = (name, genExpr body)

primOp :: Newspeak.BinOp -> Core.Primitive
primOp Newspeak.Add = Core.Add
primOp Newspeak.Sub = Core.Sub
primOp Newspeak.Mul = Core.Mul
primOp Newspeak.Div = Core.Div


