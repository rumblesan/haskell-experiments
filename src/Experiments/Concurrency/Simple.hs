module Experiments.Concurrency.Simple where

import Control.Concurrent
import Control.Monad

import Data.Monoid


data PingPong = Ping | Pong deriving (Eq, Show)

threadHandler :: MVar PingPong -> IO ()
threadHandler ppMVar = do
  val <- takeMVar ppMVar
  myId <- myThreadId
  putStrLn $ "Thread: " <> (show myId) <> " says " <> (show val)
  threadDelay 5000
  putMVar ppMVar $ case val of
    Ping -> Pong
    Pong -> Ping

pingPongThread :: MVar PingPong -> Int -> IO ThreadId
pingPongThread ppMVar times = forkIO $ replicateM_ times $ threadHandler ppMVar

simpleConcurrencyMain :: IO ()
simpleConcurrencyMain = do
  ppMVar <- newEmptyMVar
  _ <- pingPongThread ppMVar 10
  _ <- pingPongThread ppMVar 10
  _ <- pingPongThread ppMVar 10
  putMVar ppMVar Ping
  return ()

