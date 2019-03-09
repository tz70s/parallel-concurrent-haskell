{-# LANGUAGE OverloadedStrings #-}

module GetUrls where

import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Network.HTTP.Simple

getUrl :: String -> IO BSL8.ByteString
getUrl url = do
  request <- parseRequest url
  response <- httpLBS request
  return $ getResponseBody response
