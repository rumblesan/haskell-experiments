module Tests.Examples.Conway where

import           Control.Monad.State.Strict
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import           Examples.Conway

conwayTests :: Test
conwayTests =
  testGroup
    "Conway Tests"
    [ testCase "Rules" test_GOLRules
    , testCase "Blank Grid" test_blankGrid
    , testCase "Square on Grid" test_square
    , testCase "Dead Grid" test_deadGrid
    , testCase "Glider" test_glider
    ]

test_GOLRules :: Assertion
test_GOLRules =
  let ruleFunc = uncurry evolutionRules
      inputs =
        [ (Dead, [])
        , (Dead, [Alive, Alive, Alive])
        , (Alive, [])
        , (Alive, [Alive, Alive, Dead, Dead, Alive, Alive, Dead, Dead])
        , (Alive, [Dead, Dead, Alive, Alive, Dead, Dead])
        , (Alive, [Dead, Alive, Dead, Alive, Alive, Dead, Dead])
        ]
      expected = [Dead, Alive, Dead, Dead, Alive, Alive]
      results = fmap ruleFunc inputs
  in assertEqual "" expected results

test_blankGrid :: Assertion
test_blankGrid =
  let input = createGame 8 8
      output = execState (simulateBoardNTimes 10) input
  in assertEqual "" input output

test_square :: Assertion
test_square =
  let input =
        execState
          (do setCellValue (2, 2) Alive
              setCellValue (2, 3) Alive
              setCellValue (3, 2) Alive
              setCellValue (3, 3) Alive)
          (createGame 8 8)
      output = execState (simulateBoardNTimes 1) input
  in assertEqual "" input output

test_deadGrid :: Assertion
test_deadGrid =
  let emptyGame = createGame 8 8
      input = execState (do setCellValue (2, 2) Alive) emptyGame
      output = execState (simulateBoardNTimes 10) input
  in assertEqual "" emptyGame output

test_glider :: Assertion
test_glider =
  let emptyGame = createGame 6 6
      input =
        execState
          (do setCellValue (3, 1) Alive
              setCellValue (1, 2) Alive
              setCellValue (3, 2) Alive
              setCellValue (2, 3) Alive
              setCellValue (3, 3) Alive)
          emptyGame
      expected =
        execState
          (do setCellValue (4, 2) Alive
              setCellValue (2, 3) Alive
              setCellValue (4, 3) Alive
              setCellValue (3, 4) Alive
              setCellValue (4, 4) Alive)
          emptyGame
      output = execState (simulateBoardNTimes 4) input
  in assertEqual "" expected output
