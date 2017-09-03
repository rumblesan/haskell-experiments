module Main where

import           Test.Framework

import           Data.Monoid

import           Tests.Experiments.HaskVM                    (haskVMTests)
import           Tests.Experiments.MonadT.ExceptWriterStateT (exceptWriterStateParserTests)
import           Tests.Experiments.MonadT.StateParser        (transformedStateParserTests)
import           Tests.Experiments.MyTree                    (myTreeTests)
import           Tests.Experiments.StateParser               (stateParserTests)

import           Tests.Examples.Conway                       (conwayTests)

main :: IO ()
main =
  defaultMainWithOpts
    [ myTreeTests
    , haskVMTests
    , stateParserTests
    , transformedStateParserTests
    , exceptWriterStateParserTests
    , conwayTests
    ]
    mempty
