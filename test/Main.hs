module Main where

import Test.Framework

import Data.Monoid

import Tests.MyTree (myTreeTests)
import Tests.HaskVM (haskVMTests)
import Tests.StateParser (stateParserTests)


main :: IO ()
main =
  defaultMainWithOpts
    [
      myTreeTests,
      haskVMTests,
      stateParserTests
    ]
    mempty

