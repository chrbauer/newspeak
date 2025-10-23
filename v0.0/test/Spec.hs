module Main (main) where

import Test.Tasty
import qualified EvalSpec
import qualified ReplSpec

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ EvalSpec.tests
  , ReplSpec.tests
  ]
