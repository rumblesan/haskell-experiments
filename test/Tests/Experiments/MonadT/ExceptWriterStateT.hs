module Tests.Experiments.MonadT.ExceptWriterStateT where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Experiments.MonadT.ExceptWriterStateT

import Control.Monad.State.Strict
import Control.Monad.Except

exceptWriterStateParserTests :: Test
exceptWriterStateParserTests =
  testGroup "ExceptWriterStateT Tests" [
    testCase "Single expression input" test_singleExpression
  ]


test_singleExpression :: Assertion
test_singleExpression =
  let
    input = "(asdf)"
    result = parse input
    expected = (Right $ Expression "asdf", ["open paren", "close paren"])
  in
    assertEqual "" expected result


