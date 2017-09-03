module Tests.Experiments.MonadT.ExceptWriterStateT where

import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.HUnit                            (Assertion, assertEqual)

import           Experiments.MonadT.ExceptWriterStateT

import           Control.Monad.Except
import           Control.Monad.State.Strict

exceptWriterStateParserTests :: Test
exceptWriterStateParserTests =
  testGroup
    "ExceptWriterStateT Tests"
    [testCase "Single expression input" test_singleExpression]

test_singleExpression :: Assertion
test_singleExpression =
  let input = "(asdf)"
      result = parse input
      expected = (Right $ Expression "asdf", ["open paren", "close paren"])
  in assertEqual "" expected result
