

module Codegen (emitJS) where

import AST
import Data.List       (intersperse, intercalate)
import qualified Data.Map as Map

emitJS :: Program -> String
emitJS (Program bindings) =
     "const { store, fetch, update, int_print, int_gr, int_add } = require('./rts.js');\n\n"
  ++ Map.foldMapWithKey emitBinding bindings
  ++ "\nconsole.log(main());\n"

emitBinding :: Var -> Binding -> String
emitBinding name (Binding args body) =
  "function " ++ name ++ "(" ++ emitArgs args ++ ") {\n"
  ++ unlines (map ("  " ++) (emitLines body))
  ++ "}\n\n"

emitArgs :: [Var] -> String
emitArgs = concat . intersperse ", "

emitLines :: Exp -> [String]
emitLines (Bind v se rest) =
  ("let " ++ v ++ " = " ++ emitSExp se ++ ";")
  : emitLines rest
emitLines (SExp se) =
  ["return " ++ emitSExp se ++ ";"]
emitLines (Case val branches) = emitCase val branches


emitCase :: Val -> [(CPat, Exp)] -> [String]
emitCase val branches =
     ["switch (" ++ emitVal val ++ ") {"]
  ++ concatMap emitNominal branches
  ++ ["}"]
  where
    emitNominal (pat, expr) =
      let
        -- Build the `case` label and the list of pattern vars
        (lbl, vs) = case pat of
          TagNPat t vs'  -> ("case " ++ show t ++ ": {", vs')
          Tag0Pat t      -> ("case " ++ show t ++ ":",     [])
          LiteralPat n   -> ("case " ++ show n ++ ":",     [])
          _              -> error "unexpected variable‐pattern"

        -- Destructure fields _first_, if any
        extractLines
          | null vs   = []
          | otherwise = ["  const [" ++ intercalate ", " vs ++ "] = l2.fields;"]
        
        -- Body of the branch
        bodyLines    = map ("  " ++) (emitLines expr)

        -- Closing lines
        closingLines = ["  break;", "}"]
      in
        -- Concatenate the pieces:
        [lbl]
     ++ extractLines
     ++ bodyLines
     ++ closingLines


{-     

emitCase :: Val -> [(CPat, Exp)] -> [String]
emitCase val branches =
      ["switch (" ++ emitVal val ++ ") {"]
   ++ concatMap emitNominal nominalBranches
   ++ emitDefault defaultBranch
   ++ ["}"]
  where
    -- split the list once: at most one TagVarPat is allowed
    (nominalBranches, defaultBranch) =
        case span (not . isVarPat . fst) branches of
          (ns, [])              -> (ns, Nothing)
          (ns, [varBr])         -> (ns, Just varBr)
          (_,  _:_:_) -> error "Multiple variable patterns in case"

    isVarPat (TagVarPat _ _) = True
    isVarPat _               = False

    -- ordinary case labels
    emitNominal (pat, expr) =
      let (lbl, vs) = case pat of
            TagNPat t vs'  -> ("case " ++ show t ++ ": {", vs')
            Tag0Pat t      -> ("case " ++ show t ++ ":",     [])
            LiteralPat n   -> ("case " ++ show n ++ ":",     [])
            _              -> error "unexpected TagVarPat here"
          body = map ("  " ++) (emitLines expr)
          -- Destructure the `fields` array into the pattern variables:
          extractLine
            | null vs   = []
            | otherwise = ["  const [" ++ concat (intersperse ", " vs) ++ "] = l2.fields;"]
          tailLines =
             extractLine
          tailLines' = tailLines ++ ["  break;", "}"]          
      in lbl : body ++ tailLines

    -- default branch from TagVarPat (if present)
    emitDefault Nothing = []
    emitDefault (Just (TagVarPat var vs, expr)) =
      ["default: {"]
      ++ ["  const " ++ var ++ " = " ++ emitVal (SVal (Var var)) ++ ";"]
      ++ ["  // TODO: extract fields " ++ show vs | not (null vs)]
      ++ map ("  " ++) (emitLines expr)
      ++ ["  break;", "}"]
   -}   

emitVal :: Val -> String
emitVal (SVal sval)   = emitSVal sval
emitVal (Tag0 t)      = t
emitVal (TagN t _)    = t
emitVal EmptyTuple    = "undefined"

emitExp :: Exp -> String
emitExp (SExp se)           = emitSExp se
emitExp (Bind v se rest)    =
  let line = "let " ++ v ++ " = " ++ emitSExp se ++ ";"
  in line ++ "\n" ++ emitExp rest
emitExp (Case val branches) =
  "/* case inside parentheses not supported yet */"


emitSExp :: SExp -> String
emitSExp (Unit val)         = emitVal val
emitSExp (App (f:xs))        = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (Fetch p (Just i))  = "fetch("  ++ p ++ ", " ++ show i ++ ")"
emitSExp (Fetch p Nothing)  = "fetch("  ++ p ++ ")"
emitSExp (Update p val)     = "update(" ++ p ++ ", " ++ emitVal val ++ ")"
emitSExp (Exp exp)          = "(" ++ emitExp exp ++ ")"  -- ← parentheses
emitSExp (Store val) = case val of
  SVal sval            ->
    "store(" ++ emitSVal sval ++ ")"
  Tag0 tag             ->
    "store(\"" ++ tag ++ "\")"
  TagN tag fields      ->
    "store(\"" ++ tag ++ "\"," ++ emitFieldList fields ++ ")"
  EmptyTuple           ->
    "store()"
  where
    emitFieldList :: [SVal] -> String
    emitFieldList = concat . intersperse ", " . map emitSVal


emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var     v) = v

emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal
