module Language.Newspeak.PPrint where

import Prettyprinter
import Language.Newspeak.AST


pprint :: Module -> Doc ann
pprint (Module _ _ _ decls) = vsep $ map pprintDecl decls

pprintDecl :: FunDecl -> Doc ann
pprintDecl (Fun name args body) =
  pretty name <+> hsep (map pretty args) <+> equals <+> pprintExpr body


pprintExpr :: Expr -> Doc ann
pprintExpr (ExprVar name) = pretty name
pprintExpr (ExprLit lit) = pretty lit
pprintExpr (ExprApply f args) = pretty f <+> hsep (map pprintExpr args)
pprintExpr (ExprLet binds expr) = align $ pretty "let"  <+> (vsep $ map pprintDecl binds) <+> pretty "in" <+> pprintExpr expr
pprintExpr (ExprBinOp Add e1 e2) = pprintExpr e1 <+> pretty "+" <+> pprintExpr e2


