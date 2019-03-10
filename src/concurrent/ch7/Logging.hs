module Main where

import Control.Concurrent

newtype Logger = Logger (MVar LogCommand)

data LogCommand
  = Message String
  | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  mvar <- newEmptyMVar
  let l = Logger mvar
  _ <- forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger mvar) = loop
 where
  loop = do
    cmd <- takeMVar mvar
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop stopMVar -> do
        putStrLn "logger: stop"
        putMVar stopMVar ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger mvar) s = putMVar mvar (Message s)

logStop :: Logger -> IO ()
logStop (Logger mvar) = do
  stopMVar <- newEmptyMVar
  putMVar mvar (Stop stopMVar)
  takeMVar stopMVar

main :: IO ()
main = do
  l <- initLogger
  logMessage l "Hello"
  logMessage l "Bye"
  logStop l
