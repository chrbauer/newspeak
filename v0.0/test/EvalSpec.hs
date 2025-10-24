module EvalSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Newspeak (evalExpr)

tests :: TestTree
tests = testGroup "Eval Tests"
  [ testCase "2+2 = 4" $
      evalExpr "2+2" @?= 4,
    testCase "42 = 42" $  
      evalExpr "42" @?= 42,
    testCase "6*7 = 42" $  
      evalExpr "6*7" @?= 42,
    testCase "7-4 = 3"      $ evalExpr "7-4"     @?= 3,
    testCase "14//3 = 4"    $ evalExpr "14//3"   @?= 4,
    testCase "8//3+1 = 3"   $ evalExpr "8//3+1"  @?= 3
  ]
