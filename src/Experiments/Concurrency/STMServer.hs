module Experiments.Concurrency.STMServer where

import Control.Concurrent
import Control.Concurrent.STM

import Data.Monoid

import Network
import System.IO


server :: TBQueue String -> IO ThreadId
server q = forkIO $ withSocketsDo $ do
  sock <- listenOn $ PortNumber 1337
  socketHandler q sock

socketHandler :: TBQueue String -> Socket -> IO ()
socketHandler q sock = do
  (h, _, _) <- accept sock
  msg <- hGetLine h
  hClose h
  putStrLn "read message in server"
  atomically $ writeTBQueue q msg
  socketHandler q sock


stmServer :: IO ()
stmServer = do
  q <- newTBQueueIO 100
  tId <- server q
  putStrLn $ "Running server in thread " <> (show tId)
  queueReader q

queueReader :: TBQueue String -> IO ()
queueReader q = do
  msg <- atomically $ readTBQueue q
  putStrLn "got message in queue"
  putStrLn msg
  queueReader q

