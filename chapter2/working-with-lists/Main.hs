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
-- main = print $ doubleElems [10, 9 .. 2]

-- You can even use map to apply a value to a list of functions:
-- map ($ 10) [(+ 1), (* 3), (`div` 5)]
map'' f xs =
  if null xs
    then []
    else f (head xs) : map'' f (tail xs)

-- filter
--  checkGuestList, which will let us provide a list of people who will
--   be attending the party

-- We’ll make use of the built-in function elem that tells us
-- if a value is an element of a list:

checkGuestList guestList name =
  name `elem` guestList

-- Next, we’ll create a list of some friends and how much
--  their favorite meal costs:

foodCosts =
  [ ("Ren", 10.00),
    ("George", 4.00),
    ("Porter", 27.50)
  ]

-- add a function to combine our guest list and our food cost
-- list to find the budget we need for our party

-- read right to left:
-- isAttending is an explicit param,
--  is a function with one param returning bool
-- a list of tuple is the hidden param
-- computation:
-- get the list and pass through filter
-- every element: get the first element of tuple
-- filter by the (b -> bool) function isAttending passed as parameter
-- all elements thet get though filter are passed to map snd
-- map snd extract the second element of every element of the tuple list
-- the new single value list is passed to foldr + with 0 seed
-- foldr aggregate by sum the elements and return the total
-- so the function is:
-- pass a filter function and a list of tuple
-- result is the sum of second elements of the tuples that pass the filter

partyBudget isAttending =
  foldr (+) 0 . map snd . filter (isAttending . fst)

-- usage
-- partyBudget need a filter: checkGuestList
-- partyBudget need a list of tuple: foodCosts
-- checkGuestList need a list: provided by caller eg. ["Ren","George"]
-- checkGuestList need a name: provided by foodCosts popped by partyBudget

-- return the total budget: 14.0
main = print $ partyBudget (checkGuestList ["Ren", "George"]) foodCosts

-- It’s also possible to represent combinations
--  of map and filter using List Comprehensions.
double = [2 * number | number <- [0 .. 10]]

doubleOddsMultipleOf3 =
  [2 * number | number <- [0 .. 10], odd number, number `rem` 3 == 0]

-- illustrative example by building a function that will take two lists
-- of numbers and will return a list of pairs of elements in the first
-- list that are also in the second list, paired with odd elements of the
--  second list.illustrative example by building a function that will take
--  two lists of numbers and will return a list of pairs of elements in the
--  first list that are also in the second list, paired with odd elements
--  of the second list.

-- usage pairs [1..10] [2..5]
pairs as bs =
  let as' = filter (`elem` bs) as
      bs' = filter odd bs
      mkPairs a = map (\b -> (a, b)) bs'
   in concat $ map mkPairs as'

-- list comprehension version

pairs' as bs =
  [(a, b) | a <- as, a `elem` bs, b <- bs, odd b]

-- let’s go back to our dinner party budgeting function and imagine we wanted to
-- expand it to account for the fact that most guests may want to eat more than
-- one dish. Instead of including a price for the guest’s favorite meal, we’ll
-- instead get a list of a guest and the food they’ve requested. We’ll also take
--  two new functions. First, willEat will take a guest’s name and a food,
--  and will return true if the guest might want to eat that food. Second,
--   foodCost will take a food and return its price.

-- Using a list comprehension we can tersely
-- express our new party budget calculator

partyBudget' isAttending willEat foodCost guests =
  foldl (+) 0 $
    [ foodCost food
      | guest <- map fst guests,
        food <- map snd guests,
        willEat guest food,
        isAttending guest
    ]

-- zip

--  Think about how you might write a function named
--  combineLists that takes two lists and returns a list
--  of tuples, like in this example:
--  	λ combineLists [1..5] ["I","II","III","IV","V","VI","VII"]
--  	[(1,"I"),(2,"II"),(3,"III"),(4,"IV"),(5,"V")]

pairwiseSum xs ys =
  let sumElems pairs =
        let a = fst pairs
            b = snd pairs
         in a + b
   in map sumElems $ zip xs ys

pairwiseSum' xs ys = map (uncurry (+)) $ zip xs ys