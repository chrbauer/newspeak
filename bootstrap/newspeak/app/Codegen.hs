

module Codegen (emitJS) where

import AST
import Data.List       (intersperse)
import qualified Data.Map as Map

emitJS :: Program -> String
emitJS (Program bindings) =
     "const rts = require('./rts.js');\n\n"
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
  ++ concatMap emitBranch branches
  ++ ["}"]
  where
    emitBranch (pat, expr) =
      let header = case pat of
            TagNPat t _    -> "case " ++ t ++ ": {"
            TagVarPat v _  -> "case " ++ v ++ ": {"
            Tag0Pat t      -> "case " ++ t ++ ":"
            LiteralPat n   -> "case " ++ show n ++ ":"
          body = map ("  " ++) (emitLines expr)
          end  = case pat of
            TagNPat _ vs    -> ["  // fields: " ++ show vs, "  break;", "}"]
            TagVarPat _ vs  -> ["  // var‐fields: " ++ show vs, "  break;", "}"]
            _               -> ["  break;"]
      in header : body ++ end

emitVal :: Val -> String
emitVal (SVal sval)   = emitSVal sval
emitVal (Tag0 t)      = t
emitVal (TagN t _)    = t
emitVal EmptyTuple    = "undefined"

emitSExp :: SExp -> String
emitSExp (Unit sval)         = emitSVal sval
emitSExp (App  (f:xs))       = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (App  [])           = error "emitSExp: empty application"
emitSExp (Store sval)        = "rts.store(" ++ emitSVal sval ++ ")"
emitSExp (Fetch p (Just i))  = "rts.fetch(" ++ p ++ ", " ++ show i ++ ")"
emitSExp (Update p sval)     = "rts.update(" ++ p ++ ", " ++ emitSVal sval ++ ")"

emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var     v) = v

emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal
