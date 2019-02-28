module Main where

import qualified Data.Map as Map
import Control.Concurrent

type Name = String
type PhoneNumber = String
type PhoneBook = Map.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  mvar <- newMVar Map.empty
  return (PhoneBookState mvar)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState mvar) name number = do
  book <- takeMVar mvar
  -- Map.insert has signature of :: k -> v -> Map k v -> Map k v
  --
  -- Note: see book page 135 for detailed discussion on using lazy evaluation or strict one of insert.
  -- There are three alternatives,
  -- 1. lazy evaluation like this,
  --    as the lock release early and make the insert unevaluated
  --    (which may be evaluated when required, such as lookup).
  --    This will cause space leak when there's getting large chain of insertions.
  -- 2. strict evaluation: putMVar mvar $! Map.insert name number book
  --    but this make the unlock after the insertion completed.
  -- 3. hybrid (best option)
  --    release the lock in unevaluated expression,
  --    but evaluate after via seq and share the evaluation in ghc settings
  -- @
  -- let book' = Map.insert name number book
  -- putMVar mvar book'
  -- seq book' (return ())
  -- @
  putMVar mvar (Map.insert name number book)

lookup' :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup' (PhoneBookState mvar) name = do
  book <- takeMVar mvar
  -- Release lock, but we'll need to pass back the book, otherwise, the mvar is empty.
  putMVar mvar book
  -- Map.lookup has signature of :: k -> Map k v -> Maybe v
  return (Map.lookup name book)

main :: IO ()
main = do
  state <- new
  sequence_ [ insert state ("name" ++ show n) (show n) | n <- [1..10000] :: [Int]]
  lookup' state "name999" >>= print
  lookup' state "unknown" >>= print
