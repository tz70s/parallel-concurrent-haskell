module Main where

import           Control.Concurrent
import           Control.Monad      (replicateM_)
import           System.IO

-- The signature is: forkIO :: IO () -> IO ThreadId

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- Ignore thead id currently.
  _ <-
    forkIO $
    replicateM_ 100000 $ do
      hSetBuffering stdout NoBuffering
      forkIO (putChar 'A')
  replicateM_ 100000 (putChar 'B')
