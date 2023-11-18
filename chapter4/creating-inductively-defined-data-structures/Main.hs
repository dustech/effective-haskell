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

-- To test for equality, we’ll create another recursive function that
-- will traverse our data structure, but in this case we’re taking
--  two parameters, and we’ll want to traverse them at the same time.

eqPeano p p' =
  case (p, p') of
    (Z, Z) -> True
    (S n, S n') -> eqPeano n n'
    _ -> False

-- addPeano :: Peano -> Peano -> Peano

-- if first member is Z return second
addPeano Z b = b
-- addPeano a b = toPeano $ fromPeano a + fromPeano b

-- if first member is not Z
-- pop a successor from a and push a successor on b
addPeano (S a) b = addPeano a (S b)

-- Inductively Defined Lists

data List a = Empty | Cons a (List a)

toList :: [a] -> List a
toList [] = Empty
toList (x : xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

addToList :: List a -> a -> List a
addToList Empty a = Cons a Empty
addToList ls a = Cons a ls

toList' :: [a] -> List a
toList' = foldr Cons Empty

fromList' :: List a -> [a]
fromList' = listFoldr (:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ acc Empty = acc
listFoldr f acc (Cons x xs) = f x $ listFoldr f acc xs

-- exercise

-- For the sake of completeness, try implementing the following
-- functions based on their type signatures and the behavior of
--   the list functions in Prelude

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ acc Empty = acc
listFoldl f acc (Cons x xs) =
  let acc' = f acc x
   in listFoldl f acc' xs

-- listHead :: List a -> Maybe a
-- listTail :: List a -> List a
-- listReverse :: List a -> List a
-- listMap :: (a -> b) -> List a -> List b
