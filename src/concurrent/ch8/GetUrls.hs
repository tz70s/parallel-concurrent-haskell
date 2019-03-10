module GetUrls where

import Network.HTTP.Simple

import qualified Data.ByteString.Lazy.Char8 as BSL8

getUrl :: String -> IO BSL8.ByteString
getUrl url = do
  request  <- parseRequest url
  response <- httpLBS request
  return $ getResponseBody response
