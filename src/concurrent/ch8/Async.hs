module Main where

import Control.Concurrent
import Control.Exception

import GetUrls

import qualified Data.ByteString.Lazy as BSL

-- Simulate async and wait for reducing manual fork and mvar operations.
newtype Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  mvar <- newEmptyMVar
  _    <- forkIO $ do
    r <- try action -- Catch up exception and turn it into either.
    putMVar mvar r
  return (Async mvar)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async mvar) = readMVar mvar

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left  e  -> throwIO e
    Right a' -> return a'

-- | For returning the first success
waitAny :: [Async a] -> IO a
waitAny as = do
  mvar <- newEmptyMVar
  let
    forkwait a = forkIO $ do
      r <- try (wait a)
      putMVar mvar r -- fork thread and wait, wait for result and race for the first put to mvar.
  mapM_ forkwait as
  wait (Async mvar)

main :: IO ()
main = do
  a1 <- async (getUrl "http://www.google.com")
  a2 <- async (getUrl "http://www.bing.com")
  r1 <- wait a1
  r2 <- wait a2
  print (BSL.length r1, BSL.length r2)
