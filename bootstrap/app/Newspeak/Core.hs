module Newspeak.Core where


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


pprint :: CoreProgram -> String
pprint = concat . map (pprintSc . tidySc) . coreProgram

pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2
pprExpr (ELet isrec defns expr) = "let " ++ pprLet isrec defns ++ " in " ++ pprExpr expr
pprExpr (ECase expr alts) = "case " ++ pprExpr expr ++ " of " ++ pprAlts alts
pprExpr (ELam vars expr) = "\\" ++ unwords vars ++ " -> " ++ pprExpr expr


pprAExpr :: CoreExpr -> String
pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e = "(" ++ pprExpr e ++ ")"

pprLet :: IsRec -> [(Name, CoreExpr)] -> String
pprLet False defns = pprDefns defns
pprLet True defns = "rec " ++ pprDefns defns

pprDefns :: [(Name, CoreExpr)] -> String
pprDefns defns = unlines [v ++ " = " ++ pprExpr e | (v, e) <- defns]


pprAlts :: [CoreAlt] -> String
pprAlts alts = unlines [pprAlt alt | alt <- alts]

pprAlt :: CoreAlt -> String
pprAlt (tag, vars, expr) = "<" ++ show tag ++ "> " ++ unwords vars ++ " -> " ++ pprExpr expr

pprintSc :: CoreScDefn -> String
pprintSc (name, vars, expr) = name ++ " " ++ unwords vars ++ " = " ++ pprExpr expr ++ "\n"

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


