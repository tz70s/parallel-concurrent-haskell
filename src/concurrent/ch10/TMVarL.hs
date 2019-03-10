module TMVarL where

import Control.Concurrent.STM

newtype TMVarL a = TMVarL (TVar (Maybe a))

newEmptyTMVarL :: STM (TMVarL a)
newEmptyTMVarL = do
  var <- newTVar Nothing
  return (TMVarL var)

takeTMVarL :: TMVarL a -> STM a
takeTMVarL (TMVarL t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do
      writeTVar t Nothing
      return a

putTMVarL :: TMVarL a -> a -> STM ()
putTMVarL (TMVarL t) val = do
  m <- readTVar t
  case m of
    Just _  -> retry
    Nothing -> do
      writeTVar t (Just val)
      return ()
