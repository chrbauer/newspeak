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
genExpr (Newspeak.ExprBinOp Newspeak.Add e1 e2) = Core.EAp (Core.EAp (Core.EPrim Core.Add) (genExpr e1)) (genExpr e2)

-- genBindings :: Newspeak.FunDecl -> (a, Core.Expr a)
genBindings (Newspeak.Fun name [] body) = (name, genExpr body)

