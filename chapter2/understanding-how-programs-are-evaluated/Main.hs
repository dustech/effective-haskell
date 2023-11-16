module Main where

-- Haskell uses a form of laziness known as call by need.

-- co-recursive generator
numbersStartingAt n =
  n : numbersStartingAt (n + 1)

--  	numbersStartingAt 0 =
--  	  0 : <thunk>

-- 0 : numbersStartingAt 1

-- Which will give us:
--  	0 : 1 : <thunk>

main = print $ take 5 $ numbersStartingAt 0

-- Instead of calculating the boundaries of an array, you can use the cycle
-- function from Prelude to create an infinitely repeating list. Let’s look at an
-- example of this by writing a function to convert radians to degrees.
-- Our function will always return a number of degrees between 0 and 359
-- (we’ll only consider integer numbers of degrees), and instead of using modulo,
--  we’ll index into a repeating list

-- radsToDegrees :: Float -> Int
radsToDegrees radians =
  let degrees = cycle [0 .. 359]
      converted = truncate $ (radians * 360) / (2 * pi)
   in degrees !! converted

-- For practice, let’s write our own version of cycle. Our version will be really
-- cool, and we don’t want it to conflict with the existing function already named
-- cycle, so let’s call ours epicCycle:

epicCycle inputList =
  cycleHelper inputList
  where
    cycleHelper [] = epicCycle inputList
    cycleHelper (x : xs) = x : cycleHelper xs

moreEpicCycle inputList =
  inputList <> moreEpicCycle inputList

-- In this example, we’ll write a function that will find the first element of a
-- list that satisfies our predicate function and return it,
-- even if the list is infinite.

findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound
