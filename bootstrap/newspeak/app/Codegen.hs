module Codegen (emitJS) where

import           AST
import           Data.List            (intersperse)
import qualified Data.Map             as Map

-- | Generate the full JS program
emitJS :: Program -> String
emitJS (Program bindings) =
     Map.foldMapWithKey emitBinding bindings
  ++ "\nconsole.log(main());\n"

-- | Emit one function’s definition
emitBinding :: Var -> Binding -> String
emitBinding name (Binding args body) =
  "function " ++ name ++ "(" ++ emitArgs args ++ ") {\n"
  ++ unlines (map ("  " ++) (emitLines body))
  ++ "}\n\n"

-- | Emit comma-separated argument list
emitArgs :: [Var] -> String
emitArgs = concat . intersperse ", "

-- | Emit a sequence of JS statements or the final return
emitLines :: Exp -> [String]
emitLines (Bind v se rest) =
  ("const " ++ v ++ " = " ++ emitSExp se ++ ";")
  : emitLines rest
emitLines (SExp se) =
  ["return " ++ emitSExp se ++ ";"]
emitLines (Case val branches) = emitCase val branches

-- | Emit a JavaScript switch for a Case expression
emitCase :: Val -> [(CPat, Exp)] -> [String]
emitCase val branches =
     ["switch (" ++ emitVal val ++ ") {"] 
  ++ concatMap emitBranch branches
  ++ ["}"]
  where
    emitBranch :: (CPat, Exp) -> [String]
    emitBranch (pat, expr) =
      let header = case pat of
            TagNPat t _  -> "case " ++ t ++ ": {"
            Tag0Pat t    -> "case " ++ t ++ ":"
            LiteralPat n -> "case " ++ show n ++ ":"
          body   = map ("  " ++) (emitLines expr)
          end    = case pat of
            TagNPat _ vs -> 
              -- for TagNPat we might want to extract fields; placeholders for now
              ["  // fields: " ++ show vs]
              ++ body
              ++ ["  break;", "}"]
            _ ->
              body ++ ["  break;"]
      in header : end

-- | Emit simple and compound values
emitVal :: Val -> String
emitVal (SVal sval)    = emitSVal sval
emitVal (Tag0 t)       = t
emitVal (TagN t _)     = t  -- ignoring fields in switch key
emitVal EmptyTuple     = "undefined"  -- or "null" as desired

-- | Emit simple expressions
emitSExp :: SExp -> String
emitSExp (Unit sval)  = emitSVal sval
emitSExp (App (f:xs)) = emitSVal f ++ "(" ++ emitArgList xs ++ ")"
emitSExp (App [])     = error "emitSExp: empty application"

-- | Emit simple values or variables
emitSVal :: SVal -> String
emitSVal (Literal n) = show n
emitSVal (Var v)     = v

-- | Emit argument list
emitArgList :: [SVal] -> String
emitArgList = concat . intersperse ", " . map emitSVal
