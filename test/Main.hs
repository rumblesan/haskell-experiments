module Main where

import Test.Framework

import Data.Monoid

import Tests.MyTree (myTreeTests)


main :: IO ()
main =
  defaultMainWithOpts
    [myTreeTests]
    mempty

