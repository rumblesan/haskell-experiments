module Main where

import           Experiments.Applicative.CheapParser
import           Experiments.Applicative.Introspection
import           Experiments.Concurrency.Simple
import           Experiments.Concurrency.SimpleSTM
import           Experiments.Concurrency.STMServer
import           Experiments.HaskVM
import           Experiments.Misc.MergeTimePeriods
import           Experiments.MonadT.ErrorIO
import           Experiments.MonadT.StateParser
import           Experiments.MyTree
import           Experiments.StateParser

import           Examples.Conway
import           Examples.Equations.Evaluator
import           Examples.Equations.Parser

main :: IO ()
main = putStrLn "My Haskell experiments, let me show you them."
