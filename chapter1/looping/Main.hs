module Main where

-- compute fixx buzz

-- Given a number, fizzBuzzCount, return a string that contains all of the numbers
-- from one, up to and including fizzBuzzCount, except:

-- If the number is evenly divisible by 3, but not evenly divisible by 5,
-- replace it with the word “fizz”.

-- If the number is evenly divisible by 5, but not evenly divisible by 3,
-- replace it with the word “buzz”.

-- If the number is evenly divisible by both 3 and 5, replace it with the
-- word “fizzbuzz”.

-- When we’re writing recursive functions, we call the condition that causes the
-- recursion to end the base case.

fizzBuzzFor number
  | 0 == number `rem` 15 = "fizzbuzz"
  | 0 == number `rem` 5 = "buzz"
  | 0 == number `rem` 3 = "fizz"
  | otherwise = show number

-- main = putStrLn $ fizzBuzzFor 3

naiveFizzBuzz fizzBuzzCount curNum fizzBuzzString =
  if curNum > fizzBuzzCount
    then fizzBuzzString
    else
      let nextFizzBuzzString = fizzBuzzString <> fizzBuzzFor curNum <> " "
          nextNumber = curNum + 1
       in naiveFizzBuzz fizzBuzzCount nextNumber nextFizzBuzzString

main = putStrLn $ naiveFizzBuzz 3 0 ""
