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
      sexp <- emitSExp se
      let line = "let " ++ sanitizeVar v ++ " = " ++ sexp ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

    TagN _ fields -> do
      tmp <- freshPat
      sexp <- emitSExp se
      let bindNode = "const " ++ tmp ++ " = " ++ sexp ++ ";"
          vars     = [ sanitizeVar v | Var v <- fields ]
          destruct = "const [" ++ intercalate ", " vars ++ "] = " ++ tmp ++ ".fields;"
      tailLines <- emitLines rest
      return (bindNode : destruct : tailLines)

    Tag0 _ -> do
      tmp <- freshPat
      sexp <- emitSExp se
      let line = "const " ++ tmp ++ " = " ++ sexp ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

    SVal (Literal n) -> do
      let line = "const _lit = " ++ show n ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

    EmptyTuple -> do
      tmp <- freshPat
      sexp <- emitSExp se
      let line = "const " ++ tmp ++ " = " ++ sexp ++ ";"
      tailLines <- emitLines rest
      return (line : tailLines)

emitLines (SExp se) = do
  sexp <- emitSExp se
  return ["return " ++ sexp ++ ";"]

emitLines (Case val branches) =
  emitCase val branches

--------------------------------------------------------------------------------
-- | Fully-monadic case emitter
--------------------------------------------------------------------------------

emitCase :: Val -> [(CPat, Exp)] -> CG [String]
emitCase val branches = do
  branchLines <- mapM (emitBranch val) branches
  return $ ["switch (" ++ emitVal val ++ ".tag) {"] ++ concat branchLines ++ ["}"]

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

-- | Emit simple expressions (now monadic)
emitSExp :: SExp -> CG String
emitSExp (Unit val)        = return (emitVal val)
emitSExp (App (f:xs))      = do
  fn  <- return (emitSVal f)
  as  <- mapM (return . emitSVal) xs
  return $ fn ++ "(" ++ intercalate ", " as ++ ")"
                             
emitSExp (Fetch p mi)      =
   return $ "fetch(" ++ p ++ maybe "" ((", "++) . show) mi ++ ")"
emitSExp (Update p v)      =
   return $ "update(" ++ p ++ ", " ++ emitVal v ++ ")"
emitSExp (Exp exp)         = do
   body <- emitExp exp
   return $ "(" ++ body ++ ")"
emitSExp (Store val)       = case val of
   SVal sval        -> return $ "store(" ++ emitSVal sval ++ ")"
   Tag0 tag         -> return $ "store(\"" ++ tag ++ "\")"
   TagN tag fields  -> do
     fs <- return $ intercalate ", " (map emitSVal fields)
     return $ "store(\"" ++ tag ++ "\"," ++ fs ++ ")"
   EmptyTuple       -> return "store()"

  

emitExp :: Exp -> CG String
emitExp (SExp se)             = emitSExp se
emitExp (Bind lp se rest)     = do
  sexp <- emitSExp se
  let lhs = case lp of
        SVal (Var v) -> sanitizeVar v
        _            -> "_"
      line = "let " ++ lhs ++ " = " ++ sexp ++ ";"
  tail <- emitExp rest
  return (line ++ "\n" ++ tail)
emitExp (Case _ _)            = return "/* TODO: supported nested case */"

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
    x    | isAlphaNum x
         || x == '_'      -> [x]
         | otherwise       -> ""
        

-- | Generate a fresh "_patN" identifier
freshPat :: CG String
freshPat = do
  n <- get
  put (n + 1)
  return ("_pat" ++ show n)
