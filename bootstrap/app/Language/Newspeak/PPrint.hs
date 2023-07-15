module Language.Newspeak.PPrint where

import Prettyprinter
import Language.Newspeak.AST


pprint :: Module -> Doc ann
pprint (Module _ _ _ decls) = vsep $ map pprintDecl decls

pprintDecl :: Decl -> Doc ann
pprintDecl (Fun name args body) =
  pretty name <+> hsep (map pretty args) <+> equals <+> pprintExpr body


pprintExpr :: Expr -> Doc ann
pprintExpr (ExprVar name) = pretty name
pprintExpr (ExprLit lit) = pprintLit lit
pprintExpr (ExprApply f args) = pretty f <+> hsep (map pprintExpr args)
pprintExpr (ExprLet binds expr) = align $ pretty "let"  <+> (vsep $ map pprintDecl binds) <+> pretty "in" <+> pprintExpr expr
pprintExpr (ExprBinOp op e1 e2) = pprintExpr e1 <+> pprintPrimOp op <+> pprintExpr e2
pprintExpr (ExprIf cond e1 e2) = pretty "if" <+> pprintExpr cond <+> pretty "then" <+> pprintExpr e1 <+> pretty "else" <+> pprintExpr e2

pprintPrimOp :: BinOp -> Doc ann
pprintPrimOp Add = pretty "+"
pprintPrimOp Sub = pretty "-"
pprintPrimOp Mul = pretty "*"
pprintPrimOp Div = pretty "/"

-- pprintLit :: Lit -> Doc ann
pprintLit (LitInt i) = pretty i
pprintLit (LitBool b) = pretty b

