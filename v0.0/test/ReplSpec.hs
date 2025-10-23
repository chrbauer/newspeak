module ReplSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Repl (replPure)

tests :: TestTree
 tests = testGroup "REPL"
   [ testCase "simple session" $
       replPure ["2+2", ":q"] @?= ["4"]
   , testCase "multiple lines" $
       replPure ["2+2", "3+5", ":quit"] @?= ["4","8"]
   ]