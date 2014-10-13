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
    testProperty "Functor mapping works" prop_functorFmapWorks,
    testProperty "Functor fmap with id function is correct" prop_functorWithId,
    testProperty "Functor fmap is commutative" prop_functorCommutative
  ]


test_addingData :: Assertion
test_addingData =
  let
    newData = "bda"
    newTree = foldl add MyTreeEmpty newData
    expected = MyTreeTree 'b' (MyTreeLeaf 'a') (MyTreeLeaf 'd')
  in
    assertEqual "" expected newTree


prop_functorFmapWorks :: [Integer] -> Bool
prop_functorFmapWorks ints =
  let
    func = (\i -> i * 2)
    tree1 = fmap func $ foldl add MyTreeEmpty ints
    tree2 = foldl add MyTreeEmpty $ fmap func ints
  in
    tree1 == tree2

prop_functorWithId :: [Integer] -> Bool
prop_functorWithId ints =
  let
    tree1 = foldl add MyTreeEmpty ints
    tree2 = fmap id tree1
  in
    tree1 == tree2

prop_functorCommutative :: [Integer] -> Bool
prop_functorCommutative ints =
  let
    func1 = (\i -> i + 1)
    func2 = (\i -> i * 2)
    tree1 = fmap (func1 . func2) $ foldl add MyTreeEmpty ints
    tree2 = fmap func1 $ fmap func2 $ foldl add MyTreeEmpty ints
  in
    tree1 == tree2


