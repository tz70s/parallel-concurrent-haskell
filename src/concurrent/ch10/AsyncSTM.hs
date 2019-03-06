module Main where

import           Control.Concurrent     (ThreadId, forkFinally)
import           Control.Concurrent.STM (STM, TMVar, atomically,
                                         newEmptyTMVarIO, putTMVar, readTMVar,
                                         throwSTM, orElse, retry)
import           Control.Exception      (SomeException)

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
    Left e  -> throwSTM e
    Right a' -> return a'

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a)
    `orElse`
  fmap Right (waitSTM b)

waitAny :: [Async a] -> IO a
waitAny asyncs =
  -- using orElse to accumulate list of asyncs
  -- whereas retry is the zero value, and the retry is occurred for the last point of async.
  atomically $ foldr orElse retry $ map waitSTM asyncs

main :: IO ()
main = putStrLn "Hello world"
