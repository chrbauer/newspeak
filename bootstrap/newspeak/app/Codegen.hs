module Codegen (emitJS) where

import AST
import Data.List        (intercalate)
import qualified Data.Map as Map
import Data.Char        (isAlphaNum)
import Control.Monad.State

-- | Codegen monad carrying the next _pat counter
type CG = State Int

--------------------------------------------------------------------------------
-- | Top-level entry
--------------------------------------------------------------------------------

emitJS :: Program -> String
emitJS (Program bindings) =
     "const { store, fetch, update, int_print, int_gr, int_add } = require('./rts.js');\n\n"
  ++ evalState (fmap concat $ mapM (uncurry emitBinding) (Map.toList bindings)) 0
  ++ "\nconsole.log(main());\n"

--------------------------------------------------------------------------------
-- | Function bindings
--------------------------------------------------------------------------------

emitBinding :: Var -> Binding -> CG String
emitBinding name (Binding args body) = do
  linesBody <- emitLines body
  let header = "function " ++ name ++ "(" ++ intercalate ", " args ++ ") {\n"
  return $ header ++ unlines (map ("  " ++) linesBody) ++ "}\n\n"

--------------------------------------------------------------------------------
-- | Emit a sequence of lines for any Exp
--------------------------------------------------------------------------------

emitLines :: Exp -> CG [String]
emitLines (Bind lp se rest) =
  case lp of
    SVal (Var v) -> do
      let line = "let " ++ sanitizeVar v ++ " = " ++ emitSExp se ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

    TagN _ fields -> do
      tmp <- freshPat
      let bindNode = "const " ++ tmp ++ " = " ++ emitSExp se ++ ";"
          vars     = [ sanitizeVar v | Var v <- fields ]
          destruct = "const [" ++ intercalate ", " vars ++ "] = " ++ tmp ++ ".fields;"
      tailLines <- emitLines rest
      return (bindNode : destruct : tailLines)

    Tag0 _ -> do
      tmp <- freshPat
      let line = "const " ++ tmp ++ " = " ++ emitSExp se ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

    SVal (Literal n) -> do
      let line = "const _lit = " ++ show n ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

    EmptyTuple -> do
      tmp <- freshPat
      let line = "const " ++ tmp ++ " = " ++ emitSExp se ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

emitLines (SExp se) =
  return ["return " ++ emitSExp se ++ ";"]

emitLines (Case val branches) =
  emitCase val branches

--------------------------------------------------------------------------------
-- | Fully-monadic case emitter
--------------------------------------------------------------------------------

emitCase :: Val -> [(CPat, Exp)] -> CG [String]
emitCase val branches = do
  branchLines <- mapM (emitBranch val) branches
  return $ ["switch (" ++ emitVal val ++ ") {"] ++ concat branchLines ++ ["}"]

emitBranch :: Val -> (CPat, Exp) -> CG [String]
emitBranch val (pat, expr) = do
  -- label
  let (lbl, vars) = case pat of
        TagNPat t vs -> ("case " ++ show t ++ ": {", vs)
        Tag0Pat t    -> ("case " ++ show t ++ ":", [])
        LiteralPat n -> ("case " ++ show n ++ ":", [])
        _            -> error "unexpected pattern"

  -- destructuring
  let extract
        | null vars  = []
        | otherwise  = ["  const [" ++ intercalate ", " (map sanitizeVar vars)
                        ++ "] = " ++ emitVal val ++ ".fields;"]

  -- body
  bodyLines <- fmap (map ("  " ++)) (emitLines expr)

  return $ [lbl] ++ extract ++ bodyLines ++ ["  break;", "}"]

--------------------------------------------------------------------------------
-- | Primitive emitters
--------------------------------------------------------------------------------

emitVal :: Val -> String
emitVal (SVal sval) = emitSVal sval
emitVal (Tag0 t)    = t
emitVal (TagN t _)  = t
emitVal EmptyTuple  = "undefined"

emitSExp :: SExp -> String
emitSExp (Unit val)        = emitVal val
emitSExp (App (f:xs))      = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (Fetch p (Just i))  = "fetch(" ++ p ++ ", " ++ show i ++ ")"
emitSExp (Fetch p Nothing) = "fetch(" ++ p ++ ")"
emitSExp (Update p v)      = "update(" ++ p ++ ", " ++ emitVal v ++ ")"
emitSExp (Exp exp)         = "(" ++ evalState (emitExp exp) 0 ++ ")"
emitSExp (Store val)       = case val of
  SVal sval        -> "store(" ++ emitSVal sval ++ ")"
  Tag0 tag         -> "store(\"" ++ tag ++ "\")"
  TagN tag fields  -> "store(\"" ++ tag ++ "\"," ++ intercalate ", " (map emitSVal fields) ++ ")"
  EmptyTuple       -> "store()"

emitExp :: Exp -> CG String
emitExp (SExp se)             = return (emitSExp se)
emitExp (Bind lp se rest)     = do
  let lhs = case lp of
        SVal (Var v) -> sanitizeVar v
        _            -> "_"
      line = "let " ++ lhs ++ " = " ++ emitSExp se ++ ";"
  tail <- emitExp rest
  return (line ++ "\n" ++ tail)
emitExp (Case _ _)            = return "/* nested case unsupported */"

emitArgList :: [SVal] -> String
emitArgList = intercalate ", " . map emitSVal

emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var v)     = sanitizeVar v

--------------------------------------------------------------------------------
-- | Identifier & pattern helpers
--------------------------------------------------------------------------------

sanitizeVar :: String -> String
sanitizeVar = concatMap $ \c ->
  case c of
    '\'' -> "_prime"
    x    | isAlphaNum x -> [x]
         | otherwise    -> ""

-- | Generate a fresh "_patN" identifier
freshPat :: CG String
freshPat = do
  n <- get
  put (n + 1)
  return ("_pat" ++ show n)
