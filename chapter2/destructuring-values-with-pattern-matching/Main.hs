module Main where

-- Using pattern matching lets you write powerful expressions that match parts
-- of a value based on its shape.

customGreeting "George" = "Oh, hey George!"
customGreeting name = "Hello, " <> name

main = print $ customGreeting "George"

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

--  	​λ​ fancyNumbers n = (zip fibs primes) !! n
-- ​ 	​λ​ fancyNumbers 27
-- ​ 	(317811,103)

-- you can use pattern matching on the tuple within a let expression to help make
-- your code a bit easier to read

--  	printFancy n =
-- ​ 	  ​let​ (fib, prime) = fancyNumbers n
-- ​ 	      fib' = show fib
-- ​ 	      prime' = show prime
-- ​ 	  ​in​ ​"The fibonacci number is: "​ <> fib' <> ​" and the prime is: "​ <> prime'