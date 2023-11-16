module Main where

-- Haskell uses a form of laziness known as call by need.

-- co-recursive generator
numbersStartingAt n =
  n : numbersStartingAt (n + 1)

--  	numbersStartingAt 0 =
-- ​ 	  0 : <thunk>

-- 0 : numbersStartingAt 1

-- Which will give us:
-- ​ 	0 : 1 : <thunk>

main = print $ take 5 $ numbersStartingAt 0