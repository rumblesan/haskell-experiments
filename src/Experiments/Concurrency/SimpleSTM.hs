module Experiments.Concurrency.SimpleSTM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative

import Data.Monoid


threadRuns :: IO () -> Int -> IO ThreadId
threadRuns handler times = forkIO $ replicateM_ times handler

threadForever :: IO () -> IO ThreadId
threadForever handler = forkIO $ forever handler


data DataBus = DataBus {
  queue :: TBQueue Int,
  logger :: TBQueue String,
  counter :: TMVar Int
}

newDataBus :: IO DataBus
newDataBus = DataBus <$> (newTBQueueIO 100) <*> (newTBQueueIO 100) <*> (newTMVarIO 0)

readHandler :: DataBus -> IO ()
readHandler databus = do
  (value, queueLength) <- atomically $ do
    v <- readTBQueue $ queue databus
    len <- readTMVar $ counter databus
    let newLen = len - 1
    _ <- swapTMVar (counter databus) (len - 1)
    return (v, newLen)
  let logMsg = "Got a value: " <> show value <> "\nQueue length: " <> show queueLength
  atomically $ writeTBQueue (logger databus) logMsg
  threadDelay 5000

readerThread :: DataBus -> Int -> IO ThreadId
readerThread databus times = threadRuns (readHandler databus) times


writeHandler :: DataBus -> IO ()
writeHandler databus = do
  let newValue = 5 -- TODO randomise this
  queueLength <- atomically $ do
    writeTBQueue (queue databus) newValue
    len <- readTMVar $ counter databus
    let newLen = len + 1
    _ <- swapTMVar (counter databus) (len + 1)
    return newLen
  let logMsg = "Added: " <> show newValue <> "\nQueue length: " <> show queueLength
  atomically $ writeTBQueue (logger databus) logMsg
  threadDelay 2500

writerThread :: DataBus -> Int -> IO ThreadId
writerThread databus times = threadRuns (writeHandler databus) times


logHandler :: DataBus -> IO ()
logHandler databus = do
  msg <- atomically $ readTBQueue (logger databus)
  putStrLn msg

loggerThread :: DataBus -> IO ThreadId
loggerThread databus = threadForever (logHandler databus)

simpleSTMConcurrenty :: IO ()
simpleSTMConcurrenty = do
  databus <- newDataBus
  _ <- readerThread databus 10
  _ <- writerThread databus 20
  _ <- loggerThread databus
  return ()



