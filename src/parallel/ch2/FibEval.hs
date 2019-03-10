module Main
  ( main
  )
where

import Control.Parallel.Strategies
import Control.Applicative (liftA2)

-- The following example will blow out the memory size, should be fixed.

-- | Basic fib function.
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | Double fib with two spark.
doubleFibViaTwoSpark :: Integer -> Integer
doubleFibViaTwoSpark n = runEval $ do
  l <- rpar (fib n)
  r <- rpar (fib n)
  return (l + r)

parFib :: Integer -> Eval Integer
parFib 0 = return 0
parFib 1 = return 1
parFib n = do
  l <- rpar $ parFib (n - 1)
  r <- rpar $ parFib (n - 2)
  liftA2 (+) l r

doubleFibViaManySpark :: Integer -> Integer
doubleFibViaManySpark n = runEval $ do
  l <- parFib n
  r <- parFib n
  return (l + r)

main :: IO ()
main = do
  print $ doubleFibViaTwoSpark 50
  print $ doubleFibViaManySpark 50
