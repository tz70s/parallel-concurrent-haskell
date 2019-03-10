module WindowMan where

import           Control.Concurrent.STM
import           Data.Map
import qualified Data.Set               as Set

data Window = Window deriving (Eq, Show, Ord)
data Desktop = Desktop deriving (Eq, Show, Ord)

type Display = Map Desktop (TVar (Set.Set Window))

moveWindowsSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowsSTM disp win a b = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (Set.delete win wa)
  writeTVar mb (Set.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b

moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = atomically $ moveWindowsSTM disp win a b

swapWindows :: Display -> Window -> Desktop -> Window -> Desktop -> IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowsSTM disp w a b
  moveWindowsSTM disp v b a

type UserFocus = TVar Desktop

getWindows :: Display -> UserFocus -> STM (Set.Set Window)
getWindows disp focus = do
  desktop <- readTVar focus
  readTVar (disp ! desktop)

renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus
  loop wins
  where
    loop wins = do
      putStrLn $ "Render window " <> show wins
      next <- atomically $ do
        wins' <- getWindows disp focus
        if (wins == wins')
        then retry
        else return wins'
      loop next

