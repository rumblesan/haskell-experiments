module Main where

import Test.Framework

import Data.Monoid

import Tests.MyTree (myTreeTests)
import Tests.HaskVM (haskVMTests)


main :: IO ()
main =
  defaultMainWithOpts
    [
      myTreeTests,
      haskVMTests
    ]
    mempty

