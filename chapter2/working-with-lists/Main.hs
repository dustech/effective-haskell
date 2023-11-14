module Main where

import Prelude hiding (foldl, foldr)

listOfNums = [1, 2, 3]

listOfFloats = [1.1, 2.2, 3.3]

listOfStrings = ["hello", "world"]

isTrueHello = ['h', 'e', 'l', 'l', 'o'] == "hello"

words = ["foo", "bar", "baz", "fizz", "buzz"]

-- You can get the nth element of a list using the (!!) operator
-- words !! 0

-- list cosntructing examples

-- 1 : [2, 3]
-- 1 : 2 : [3]
-- 1 : 2 : 3 : []
-- 'h' : "ello"
-- 'h' : 'e' : ['l', 'l', 'o']
-- [1, 2, 3] : []
-- [1] : [2] : [3] : []

-- When you add an element to the front of a list, we call the part that you are
--  adding the head of the list. In fact, a common alternative phrase cons-ing is
--   to push an element onto the head of a list. The list that you are adding the
--   element onto becomes the tail.

-- Head and tail aren’t just the terms we use to talk about parts of a list.
-- The head and tail functions let you deconstruct a list and get the first
--  element and the rest of the elements back out of a list you’ve constructed.
--  Let’s look at some examples of using head and tail so you can get a feel for
--   how they work:

--   λ head [1,2,3]
--  	1
--  	λ tail [1,2,3]
--  	[2,3]
--  	λ head (tail [1,2,3])
--  	2
--  	λ tail (tail [1,2,3])
--  	[3]
--  	λ tail [1]
--  	[]

-- In the case of head and tail, these functions will cause a runtime exception
-- if you use them on an empty list:
--  	λ head []
--  	*** Exception: Prelude.head: empty list
--  	λ tail []
--  	*** Exception: Prelude.tail: empty list

--  You can check for an empty list with equality:
--  	listIsEmpty list =
--  	  if list == []
--  	  then putStrLn "this list is empty"
--  	  else putStrLn ("the first element of this list is: " <> show (head list))

-- the base case is the end of the recursion

-- In this example, you start with a base case where n is less than or equal to 0.
-- In the base case, you return an empty list. If n is greater than zero,
-- you construct a list by prepending the current element to a recursive call
-- that decrements the variable. This is a bit different than some of the recursive
--  examples that you saw in the last chapter, because you’re building up a value
--   rather than reducing one.

countdown n =
  if n <= 0
    then []
    else n : countdown (n - 1)

factors num =
  factors' num 2
  where
    factors' num fact
      | num <= 1 = []
      | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
      | otherwise = factors' num (fact + 1)

-- main = print $ factors 10

isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)

-- main = print $ isBalanced "hello ( my friend )"
-- You’ll notice when you look at this code that it looks almost like a minimum
-- viable example of what a recursive function should look like. In the Haskell
--  community, when we’re talking about the essential behavior of a function or a
--   datatype, without any extraneous business logic or implementation details,
--   we sometimes refer to that as the shape of the function or data structure.
--   In this case, we might say that this function has the shape of any general
--    recursive function over a list.

reduce func carryValue lst =
  if null lst
    then carryValue
    else
      let intermediateValue = func carryValue (head lst)
       in reduce func intermediateValue (tail lst)

-- main = print $ reduce (+) 0 [1 .. 10]

isBalanced' str = 0 == reduce checkBalance 0 str
  where
    checkBalance count letter
      | letter == '(' = count + 1
      | letter == ')' = count - 1
      | otherwise = count

-- main = print $ isBalanced' "my unbalanced string ("

foldl func carryValue lst =
  if null lst
    then carryValue
    else foldl func (func carryValue (head lst)) (tail lst)

foldr func carryValue lst =
  if null lst
    then carryValue
    else func (head lst) $ foldr func carryValue (tail lst)

-- main =
--   print $
--     show (foldl (+) 0 [1 .. 100])
--       <> "="
--       <> show (foldr (+) 0 [1 .. 100])

doubleElems nums =
  if null nums
    then []
    else
      let hd = head nums
          tl = tail nums
       in (2 * hd) : doubleElems tl

-- The map function takes a function and applies it to every element in a list
main = print $ doubleElems [10, 9 .. 2]

-- You can even use map to apply a value to a list of functions:
-- map ($ 10) [(+ 1), (* 3), (`div` 5)]
map'' f xs =
  if null xs
    then []
    else f (head xs) : map'' f (tail xs)