module Main where

main = print ""

-- Most data structures that you’ll build in Haskell are recursive,
--  or as we sometimes call them, inductively defined.

-- Counting With Peano Numbers

-- The simplest type of inductively defined data structure that you can
-- implement is a type that represents a peano number. Peano numbers are
-- a way of representing whole numbers as a recursive function, where a
-- given peano number is either “zero” or “a successor to a peano number.”

-- Z zero, S successor

data Peano = Z | S Peano

-- Both are recursive functions with a base case at zero. The successor
-- functions in both cases reduce their input while growing their output.
--  In toPeano each recursive call adds a new S constructor. In fromPeano
--   we’re using the succ function, from Prelude,
--   which increments an enumeration.

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

showPeano p = "Peano " <> show (fromPeano p)
