module TQueueL where

import Control.Concurrent.STM
import Prelude hiding (read)

-- | The basic idea is that we've a read side queue and a write side queue.
-- while the read side queue is empty, reverse the write side queue to be read side,
-- and clean up the write side.
-- TQueue is outperform than TChan and MVar-based Chan.
data TQueueL a = TQueueL (TVar [a]) (TVar [a])

newTQueueL :: STM (TQueueL a)
newTQueueL = do
  read  <- newTVar []
  write <- newTVar []
  return (TQueueL read write)

writeTQueueL :: TQueueL a -> a -> STM ()
writeTQueueL (TQueueL _ write) a = do
  xs <- readTVar write
  writeTVar write (a : xs)

readTQueueL :: TQueueL a -> STM a
readTQueueL (TQueueL read write) = do
  xs <- readTVar read
  case xs of
    (x : xs') -> do
      writeTVar read xs'
      return x
    [] -> do
      ys <- readTVar write
      case ys of
        -- when both are empty
        -- The reverse is done lazily,
        -- if the reverse is done strictly,
        -- the STM (atomic operation) will take much more time until reverse is done.
        -- This will make the operations slower.
        [] -> retry
        _  -> do
          let (z : zs) = reverse ys
          writeTVar write []
          writeTVar read  zs
          return z
