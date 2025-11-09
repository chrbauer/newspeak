module EvalSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import Newspeak.Grin.Parser (parseExpr)
import Newspeak.Grin.Ast (Value(..))
import Newspeak.Eval (eval)
import Newspeak.Module (emptyModule)
import           Control.Monad.Except


-- helper: parse + evaluate expression
evalExpr :: String -> Int
evalExpr src =
  case parseExpr src of
    Left err  -> error err
    Right ast ->
      case runExcept (eval emptyModule M.empty ast) of
        Left err -> error (show err)
        Right (VInt n) -> n

tests :: TestTree
tests = testGroup "Eval Tests"
  [ testCase "2+2 = 4"      $ evalExpr "2+2"     @?= 4
  , testCase "42 = 42"      $ evalExpr "42"      @?= 42
  , testCase "6*7 = 42"     $ evalExpr "6*7"     @?= 42
  , testCase "7-4 = 3"      $ evalExpr "7-4"     @?= 3
  , testCase "14//3 = 4"    $ evalExpr "14//3"   @?= 4
  , testCase "8//3+1 = 3"   $ evalExpr "8//3+1"  @?= 3
  ]
