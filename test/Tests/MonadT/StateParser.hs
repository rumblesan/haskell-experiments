module Tests.MonadT.StateParser where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Experiments.MonadT.StateParser
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

transformedStateParserTests :: Test
transformedStateParserTests =
  testGroup "StateT Tests" [
    testCase "Single expression input" test_singleExpression
  ]


test_singleExpression :: Assertion
test_singleExpression =
  let
    input = myLex "(asdf)"
    result = evalState (runExceptT myParse) input
    expected = Right $ Expression "asdf"
  in
    assertEqual "" expected result

