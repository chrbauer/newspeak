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
      evalExpr "6*7" @?= 42
  ]
