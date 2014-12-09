module Tests.StateParser where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Experiments.StateParser
import Control.Monad.State.Lazy (evalState)

stateParserTests :: Test
stateParserTests =
  testGroup "StateParser Tests" [
    testCase "Single expression input" test_singleExpression
  ]


test_singleExpression :: Assertion
test_singleExpression =
  let
    input = myLex "(asdf)"
    result = evalState myParse input
    expected = Expression "asdf"
  in
    assertEqual "" expected result


