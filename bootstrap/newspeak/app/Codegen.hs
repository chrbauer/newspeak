module Codegen (emitJS) where

import AST
import Data.List       (intersperse, intercalate)
import qualified Data.Map as Map
import Data.Char       (isAlphaNum)

--------------------------------------------------------------------------------
-- | Top-level entry
--------------------------------------------------------------------------------

emitJS :: Program -> String
emitJS (Program bindings) =
     "const { store, fetch, update, int_print, int_gr, int_add } = require('./rts.js');\n\n"
  ++ Map.foldMapWithKey emitBinding bindings
  ++ "\nconsole.log(main());\n"

--------------------------------------------------------------------------------
-- | Function bindings
--------------------------------------------------------------------------------

emitBinding :: Var -> Binding -> String
emitBinding name (Binding args body) =
  "function " ++ name ++ "(" ++ emitArgs args ++ ") {\n"
  ++ unlines (map ("  " ++) (emitLines body))
  ++ "}\n\n"

emitArgs :: [Var] -> String
emitArgs = concat . intersperse ", "

--------------------------------------------------------------------------------
-- | Line-wise emitter
--------------------------------------------------------------------------------

emitLines :: Exp -> [String]
emitLines (Bind lp se rest) =
  case lp of
    -- simple variable binder
    SVal (Var v) ->
      [ "let " ++ sanitizeVar v ++ " = " ++ emitSExp se ++ ";" ]
      ++ emitLines rest

    -- constructor pattern: allocate tmp, then destructure fields
    TagN tag fields ->
      let tmp      = "_pat"
          bindNode = "const " ++ tmp ++ " = " ++ emitSExp se ++ ";"
          varNames = [ sanitizeVar v | Var v <- fields ]
          destruct = "const [" ++ intercalate ", " varNames ++ "] = " ++ tmp ++ ".fields;"
      in  [bindNode, destruct]
       ++ emitLines rest

    -- nullary constructor
    Tag0 tag ->
      let tmp = "_pat"
      in [ "const " ++ tmp ++ " = " ++ emitSExp se ++ ";" ]
       ++ emitLines rest

    -- literal pattern
    SVal (Literal n) ->
      [ "const _lit = " ++ show n ++ ";" ]
      ++ emitLines rest

    -- empty tuple
    EmptyTuple ->
      [ "const _pat = " ++ emitSExp se ++ ";" ]
      ++ emitLines rest

emitLines (SExp se) =
  ["return " ++ emitSExp se ++ ";"]

emitLines (Case val branches) =
  emitCase val branches

--------------------------------------------------------------------------------
-- | Case expression emitter
--------------------------------------------------------------------------------

emitCase :: Val -> [(CPat, Exp)] -> [String]
emitCase val branches =
     ["switch (" ++ emitVal val ++ ") {"]
  ++ concatMap emitNominal branches
  ++ ["}"]
 where
  emitNominal (pat, expr) =
    let
      -- case label and vars
      (lbl, vs) = case pat of
        TagNPat t vs' -> ("case " ++ show t ++ ": {", vs')
        Tag0Pat t     -> ("case " ++ show t ++ ":",     [])
        LiteralPat n  -> ("case " ++ show n ++ ":",     [])
        _             -> error "unexpected pattern"

      -- destructure if needed
      extractLines
        | null vs   = []
        | otherwise = ["  const [" ++ intercalate ", " (map sanitizeVar vs)
                       ++ "] = l2.fields;"]

      bodyLines    = map ("  " ++) (emitLines expr)
      closingLines = ["  break;", "}"]
    in
      [lbl]
   ++ extractLines
   ++ bodyLines
   ++ closingLines

--------------------------------------------------------------------------------
-- | Primitive emitters
--------------------------------------------------------------------------------

emitVal :: Val -> String
emitVal (SVal sval)   = emitSVal sval
emitVal (Tag0 t)      = t
emitVal (TagN t _)    = t
emitVal EmptyTuple    = "undefined"

emitExp :: Exp -> String
emitExp (SExp se)                       = emitSExp se
emitExp (Bind (SVal (Var v)) se rest)   =
  let line = "let " ++ sanitizeVar v ++ " = " ++ emitSExp se ++ ";"
  in line ++ "\n" ++ emitExp rest
emitExp (Case _ _) =
  "/* nested case in expression not supported yet */"

emitSExp :: SExp -> String
emitSExp (Unit val)          = emitVal val
emitSExp (App (f:xs))        = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (Fetch p (Just i))  = "fetch("  ++ p ++ ", " ++ show i ++ ")"
emitSExp (Fetch p Nothing)   = "fetch("  ++ p ++ ")"
emitSExp (Update p val)      = "update(" ++ p ++ ", " ++ emitVal val ++ ")"
emitSExp (Exp exp)           = "(" ++ emitExp exp ++ ")"
emitSExp (Store val)         = case val of
  SVal sval          -> "store(" ++ emitSVal sval ++ ")"
  Tag0 tag           -> "store(\"" ++ tag ++ "\")"
  TagN tag fields    -> "store(\"" ++ tag ++ "\"," ++ emitFieldList fields ++ ")"
  EmptyTuple         -> "store()"
 where
  emitFieldList :: [SVal] -> String
  emitFieldList = concat . intersperse ", " . map emitSVal

emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var     v) = sanitizeVar v

emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal

--------------------------------------------------------------------------------
-- | Identifier sanitization
--------------------------------------------------------------------------------

sanitizeVar :: String -> String
sanitizeVar = concatMap replace
  where
    replace '\'' = "_prime"
    replace c
      | isAlphaNum c = [c]
      | otherwise    = ""
