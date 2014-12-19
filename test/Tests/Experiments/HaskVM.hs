module Tests.Experiments.HaskVM where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Experiments.HaskVM

haskVMTests :: Test
haskVMTests =
  testGroup "HaskVM Tests" [
    testCase "Simple program" test_simpleProgram
  ]


test_simpleProgram :: Assertion
test_simpleProgram =
  let
    program = do
      push 3
      push 4
      add
      pop
    result = interpret program $ VMState [1,2]
    expected = 7
  in
    assertEqual "" expected result

