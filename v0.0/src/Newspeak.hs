module Newspeak
  ( Value(..)
  , Module
  , emptyModule
  , insertDef
  , eval
  , evalTop
  , compileToGrin
  , runGrin
  ) where

import qualified Data.Map.Strict as M
import           Newspeak.Grin.Ast
import           Newspeak.Grin.Parser (parseExpr)
import           Newspeak.Module (Module, emptyModule, insertDef)
import           Newspeak.Eval (eval, evalTop)
import           Newspeak.Repl (runReplIO)
import           Control.Monad.Except


-- | Parse source into GRIN expression
compileToGrin :: String -> Either String GExpr
compileToGrin = parseExpr

-- | Run a GRIN expression in an empty module and environment
runGrin :: GExpr -> Either String Value
runGrin e = case runExcept (eval emptyModule M.empty e) of
  Left err -> Left (show err)
  Right v -> Right v


repl :: IO ()
repl = runReplIO
