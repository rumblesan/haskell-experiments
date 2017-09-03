module Experiments.MonadT.ErrorIO where

import           Control.Monad.Except
import           System.Random

getRandomNumber :: IO Int
getRandomNumber = randomRIO (0, 20)

data RandError =
  RandError Int
            String
  deriving (Show)

type ErrorIO a = ExceptT RandError IO a

errIfOdd :: ErrorIO Int
errIfOdd = do
  n <- lift getRandomNumber
  if (n `mod` 2) == 1
    then throwError $ RandError n "odd number error"
    else return n

errIfOver :: Int -> ErrorIO Int
errIfOver lim = do
  n <- lift getRandomNumber
  if n > lim
    then throwError $ RandError n "over limit"
    else return n

errIfUnder :: Int -> ErrorIO Int
errIfUnder lim = do
  n <- lift getRandomNumber
  if n < lim
    then throwError $ RandError n "under limit"
    else return n

getSomeNumbers :: ErrorIO [Int]
getSomeNumbers = do
  n1 <- errIfOdd
  n2 <- errIfOver 10
  n3 <- errIfUnder 10
  return [n1, n2, n3]

getSomeNumbersEither :: IO (Either RandError [Int])
getSomeNumbersEither = runExceptT getSomeNumbers

alwaysPass :: ErrorIO [Int]
alwaysPass = do
  n1 <- errIfOver 30
  n2 <- errIfUnder 0
  n3 <- errIfOver 30
  return [n1, n2, n3]

alwaysPassEither :: IO (Either RandError [Int])
alwaysPassEither = runExceptT alwaysPass

alwaysFail :: ErrorIO [Int]
alwaysFail = do
  n1 <- errIfOver 30
  n2 <- errIfUnder 0
  n3 <- errIfOver 0
  return [n1, n2, n3]

alwaysFailEither :: IO (Either RandError [Int])
alwaysFailEither = runExceptT alwaysFail
