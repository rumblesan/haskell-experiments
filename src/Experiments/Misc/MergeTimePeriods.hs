module Experiments.Misc.MergeTimePeriods where

import qualified Data.List   as L
import           Data.Monoid

data Period = Period
  { start :: Int
  , end   :: Int
  } deriving (Show, Eq, Ord)

instance Monoid Period where
  mempty = Period 0 0
  mappend (Period startA endA) (Period startB endB) =
    Period (min startA startB) (max endA endB)

mergePeriods :: (Period -> Period -> Bool) -> [Period] -> [Period]
mergePeriods closeTest input = foldl merge [] input
  where
    merge :: [Period] -> Period -> [Period]
    merge [] next = [next] -- nothing parsed yet so just return the next period
    merge (x:xs) next =
      if closeTest x next
        then (x <> next) : xs
        else next : x : xs

closeEnough :: Int -> Period -> Period -> Bool
closeEnough maxTimeBetween first second =
  ((start second) - maxTimeBetween) <= (end first)

mergeClosePeriods :: Int -> [Period] -> [Period]
mergeClosePeriods maxTimeBetween input =
  let sortedPeriods = L.sort input
      closeTest = closeEnough maxTimeBetween
      mergeOp = L.reverse . mergePeriods closeTest
  in mergeOp sortedPeriods
