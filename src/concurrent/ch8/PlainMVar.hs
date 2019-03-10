module PlainMVar where

import Control.Concurrent

import GetUrls

import qualified Data.ByteString.Lazy as BSL

runGetUrl :: MVar BSL.ByteString -> String -> IO ThreadId
runGetUrl var url = forkIO $ do
  r <- getUrl url
  putMVar var r

runGetUrls :: IO ()
runGetUrls = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  _  <- runGetUrl m1 "http://www.wikipedia.org/wiki/Shovel"
  _  <- runGetUrl m2 "http://www.wikipedia.org/wiki/Spade"
  r1 <- takeMVar m1
  r2 <- takeMVar m2
  print (BSL.length r1, BSL.length r2)
