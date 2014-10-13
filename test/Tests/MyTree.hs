module Tests.MyTree where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Experiments.MyTree


myTreeTests :: Test
myTreeTests =
  testGroup "MyTree Tests" [
    testCase "Adding data works as expected" test_addingData,
    testProperty "Functor mapping works" prop_functorFmapWorks
  ]


test_addingData :: Assertion
test_addingData =
  let
    newData = "bda"
    newTree = foldl addValue MyTreeEmpty newData
    expected = MyTreeTree 'b' (MyTreeLeaf 'a') (MyTreeLeaf 'd')
  in
    assertEqual "" expected newTree


prop_functorFmapWorks :: [Integer] -> Bool
prop_functorFmapWorks ints =
  let
    func = (\i -> i * 2)
    tree1 = fmap func $ foldl addValue MyTreeEmpty ints
    tree2 = foldl addValue MyTreeEmpty $ fmap func ints
  in
    tree1 == tree2


