module Tests.Experiments.MonadT.StateParser where

import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertEqual)

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Experiments.MonadT.StateParser

transformedStateParserTests :: Test
transformedStateParserTests =
  testGroup
    "StateT Tests"
    [testCase "Single expression input" test_singleExpression]

test_singleExpression :: Assertion
test_singleExpression =
  let input = "(asdf)"
      result = parse input
      expected = Right $ Expression "asdf"
  in assertEqual "" expected result
