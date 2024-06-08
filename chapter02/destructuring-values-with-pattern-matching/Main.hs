module Main where

import Prelude hiding (fst, snd)

-- Using pattern matching lets you write powerful expressions that match parts
-- of a value based on its shape.

customGreeting "George" = "Oh, hey George!"
customGreeting name = "Hello, " <> name

-- main = print $ customGreeting "George"

matchNumber 0 = "zero"
matchNumber n = show n

matchList [1, 2, 3] = "one, two, three"
matchList list = show list

matchTuple ("hello", "world") = "greetings"
matchTuple tuple = show tuple

matchBool True = "yep"
matchBool bool = "this must be false"

matchTuple' ("hello", "world") = "Hello there, you great big world"
matchTuple' ("hello", name) = "Oh, hi there, " <> name
matchTuple' (salutation, "George") = "Oh! " <> salutation <> " George!"
matchTuple' n = show n

addValues [] = 0
addValues (first : rest) = first + (addValues rest)

-- You can also use pattern matching outside of the parameters of a function.

-- Imagine that you have a function, fancyNumbers, which given some number, n,
--  gives you back the nth Fibonacci number and the nth prime number

--  	λ fancyNumbers n = (zip fibs primes) !! n
--  	λ fancyNumbers 27
--  	(317811,103)

-- you can use pattern matching on the tuple within a let expression to help make
-- your code a bit easier to read

--  	printFancy n =
--  	  let (fib, prime) = fancyNumbers n
--  	      fib' = show fib
--  	      prime' = show prime
--  	  in "The fibonacci number is: " <> fib' <> " and the prime is: " <> prime'

-- In some cases you want to pattern match, but also get the original value
-- that hasn’t been deconstructed. You can do that by adding a variable before
--  your pattern followed by an @ symbol.

modifyPair p@(a, b)
  | a == "Hello" = "this is a salutation"
  | b == "George" = "this is a message for George"
  | otherwise = "I don't know what " <> show p <> " means"

--  A wildcard pattern will match any value, like a variable would,
--  but without binding the value to a variable in your function.
--  It’s a useful way of saying, “a value should be here,
--   but I don’t care about it.”

fst (x, _, _) = x

snd (_, x, _) = x

thrd (_, _, x) = x

-- In some cases, you might want to both ignore a particular value,
-- as well as communicate to other developers working in your codebase
--  what that value should be. In those cases, it’s common to use a
--  variable name that starts with an underscore prefix:

printHead [] = "empty!"
printHead lst@(hd : _tail) =
  "the head of " <> (show lst) <> " is " <> show hd

--  A case statement allows you to pattern match on a value inside
--  of your function.

favoriteFood person =
  case person of
    "Ren" -> "Tofu"
    "Rebecca" -> "Falafel"
    "George" -> "Banana"
    name -> "I Don't Know what " <> name <> " likes!"

handleNums l =
  case l of
    [] -> "An empty list"
    [y]
      | y == 0 -> "a list called: [0]"
      | y == 1 -> "a singular list of [1]"
      | even y -> "a singleton list containing an even number"
      | otherwise -> "the list contains " <> (show y)
    _list -> "the list has more than 1 element"

-- Mixing case statements with guards can sometimes allow you to write
-- terse code when you are dealing with complex business logic, but be
-- careful to not use so many clauses in one place that it makes your
-- code less readable.

partialFunction 0 = "I only work for 0"
partialFunction impossibleValue =
  error $
    "I only work with 0 but I was called with " <> show impossibleValue

-- One way that we can get warnings about non-exhaustive
-- patterns is to ask for them explicitly by passing
-- in the -Wincomplete-patterns option to ghc before
-- the name of the file we’re compiling:
partialFunction' 0 = "I only work for 0"

-- we can temporarily disable the warning by passing in -Wno-incomplete-patterns
main = putStrLn $ partialFunction 3

-- GHC supports quite a lot of different warnings that can be individually enabled
-- and disabled. The pattern we just used holds for all of them. You can enable a
-- warning with -Wwarning-name and disable it with -Wno-warning-name. If you want
-- to enable all warnings about things that might be problems with your program,
-- you can also use -Wall. GHC supports quite a lot of different warnings that can
-- be individually enabled and disabled. The pattern we just used holds for all of
--   them. You can enable a warning with -Wwarning-name and disable it
--   with -Wno-warning-name. If you want to enable all warnings about things
--   that might be problems with your program, you can also use -Wall.