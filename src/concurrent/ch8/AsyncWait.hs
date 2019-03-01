module AsyncWait where

import Control.Concurrent
import Control.Exception

-- Simulate async and wait for reducing manual fork and mvar operations. 

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  mvar <- newEmptyMVar
  _ <- forkIO $ do
    r <- try action -- Catch up exception and turn it into either.
    putMVar mvar r
  return (Async mvar)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async mvar) = readMVar mvar

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right a' -> return a'

-- | For returning the first success
waitAny :: [Async a] -> IO a
waitAny as = do
  mvar <- newEmptyMVar
  let forkwait a = forkIO $ do r <- try (wait a); putMVar mvar r -- fork thread and wait, wait for result and race for the first put to mvar.
  _ <- traverse forkwait as -- or mapM_
  wait (Async mvar)
