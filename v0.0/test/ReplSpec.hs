module ReplSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Newspeak.Repl (replPure)

tests :: TestTree
tests = testGroup "REPL"
  [ testCase "simple session" $
      replPure ["2+2", ":q"] @?= ["4"]

  , testCase "multiple lines" $
      replPure ["2+2", "3+5", ":quit"] @?= ["4","8"]

  , testCase "undefined variable reported" $
      replPure ["2+x", ":q"] @?= ["error: unbound variable: x"]

  , testCase "define and use square" $
      replPure ["square x = x * x", "square 5", ":q"]
        @?= ["25"]
  ]