module Main where

import           Control.Concurrent
import           Control.Monad      (forever)

-- | Please see p.136 for this data structure design.
-- In short, this is a linked list with MVar pointer.
data ChanL a = ChanL (MVar (Stream a)) (MVar (Stream a))

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

newChanL :: IO (ChanL a)
newChanL = do
  hole <- newEmptyMVar
  -- Read side, the first pointer
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (ChanL readVar writeVar)

readChanL :: ChanL a -> IO a
readChanL (ChanL readVar _) = do
  stream <- takeMVar readVar
  Item val tailL <- readMVar stream
  putMVar readVar tailL
  return val

writeChanL :: ChanL a -> a -> IO ()
writeChanL (ChanL _ writeVar) val = do
  -- The write is write from writeVar (the end) and add new one.
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole

dupChanL :: ChanL a -> IO (ChanL a)
dupChanL (ChanL _ writeVar) = do
  -- The pointed last point.
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (ChanL newReadVar writeVar)

main :: IO ()
main = do
  putStrLn "Hello world"
  semaphore <- newEmptyMVar :: IO (MVar ())
  chan <- newChanL
  _ <- traverse (writeChanL chan) ([1..10] :: [Int])
  _ <- forkIO $ forever $ do
    val <- readChanL chan
    putStrLn $ "Take value from channel : " <> show val
    if val == 10
    then putMVar semaphore ()
    else return ()
  takeMVar semaphore
  putStrLn "End of program."
