module Main where

import Experiments.MyTree
import Experiments.HaskVM
import Experiments.StateParser
import Experiments.MonadT.StateParser
import Experiments.Concurrency.Simple
import Experiments.Concurrency.SimpleSTM
import Experiments.Misc.MergeTimePeriods
import Experiments.Applicative.Introspection

import Examples.Equations.Parser
import Examples.Equations.Evaluator
import Examples.Conway

main :: IO ()
main = putStrLn "My Haskell experiments, let me show you them."

