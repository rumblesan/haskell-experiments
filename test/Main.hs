module Main where

import Test.Framework

import Data.Monoid

import Tests.MyTree (myTreeTests)
import Tests.HaskVM (haskVMTests)
import Tests.StateParser (stateParserTests)
import Tests.MonadT.StateParser (transformedStateParserTests)


main :: IO ()
main =
  defaultMainWithOpts
    [
      myTreeTests,
      haskVMTests,
      stateParserTests,
      transformedStateParserTests
    ]
    mempty

