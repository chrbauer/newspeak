{-# LANGUAGE OverloadedStrings #-}

module Newspeak.Lang.Core where

import Prettyprinter
import Prettyprinter.Render.String
type IsRec = Bool
type Name = String
type Tag = Int
data Data = Block { tag :: Int, fields :: Int }
type Alter a = (Tag, [a], Expr a)
type CoreAlt = Alter Name

data Expr a = EVar Name | ENum Int | EConstr Int Int | EAp (Expr a) (Expr a)
  | ELet IsRec [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show, Eq)

type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name
type CoreExpr = Expr Name

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False


bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e = False


preludeDefs :: CoreProgram
preludeDefs = [
  ("I", ["x"], EVar "x"),
  ("K", ["x", "y"], EVar "x"),
  ("K1", ["x", "y"], EVar "y"),
  ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
    (EAp (EVar "g") (EVar "x"))),
  ("compose", ["f", "g", "x"], EAp (EVar "f")
    (EAp (EVar "g") (EVar "x"))),
  ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]


pprint :: CoreProgram -> Doc a
pprint = vsep . map (pprintSc . tidySc) . coreProgram

pprExpr :: CoreExpr -> Doc a
pprExpr (ENum n) = viaShow n
pprExpr (EVar v) = pretty v
pprExpr (EAp e1 e2) = pprExpr e1 <+> pprAExpr e2
pprExpr (ELet isrec defns expr) = sep ["let", pprLet isrec defns, "in", pprExpr expr]
pprExpr (ECase expr alts) = hang 4 $ hsep ["case" <+> pprExpr expr <+> "of", pprAlts alts]
pprExpr (ELam vars expr) = "\\" <+> sep (map pretty vars) <+> "->" <+> pprExpr expr


pprAExpr :: CoreExpr -> Doc a
pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e = "(" <> pprExpr e <> ")"

pprLet :: IsRec -> [(Name, CoreExpr)] -> Doc a
pprLet False defns = pprDefns defns
pprLet True defns = "rec" <+> pprDefns defns

pprDefns :: [(Name, CoreExpr)] -> Doc a
pprDefns defns = hsep [pretty v <+> "=" <+> pprExpr e | (v, e) <- defns]


pprAlts :: [CoreAlt] -> Doc a
pprAlts alts = hsep [pprAlt alt | alt <- alts]

pprAlt :: CoreAlt -> Doc a
pprAlt (tag, vars, expr) = "<" <> viaShow tag <> "> " <+> sep (map pretty vars) <+> "->" <+> pprExpr expr

pprintSc :: CoreScDefn -> Doc a
pprintSc (name, vars, expr) = pretty name <+> sep (map pretty vars) <+> "=" <+> pprExpr expr

tidySc :: CoreScDefn -> CoreScDefn
tidySc (name, vars, expr) = (name, vars, tidyExpr expr)

tidyExpr :: CoreExpr -> CoreExpr
tidyExpr (ENum n) = ENum n
tidyExpr (EVar v) = EVar v
tidyExpr (EAp e1 e2) = EAp (tidyExpr e1) (tidyExpr e2)
tidyExpr (ELet isrec defns expr) = ELet isrec [(v, tidyExpr e) | (v, e) <- defns] (tidyExpr expr)
tidyExpr (ECase expr alts) = ECase (tidyExpr expr) [(tag, vars, tidyExpr e) | (tag, vars, e) <- alts]
tidyExpr (ELam vars expr) = ELam vars (tidyExpr expr)

coreProgram :: Program Name -> Program Name
coreProgram = map tidySc


mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n $ repeat e2)


