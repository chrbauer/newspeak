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
  , testCase "parse error reported" $
      replPure ["2+x", ":q"] @?= ["error: <input>:1:3:\n  |\n1 | 2+x\n  |   ^\nunexpected 'x'\nexpecting integer\n"]
  ]