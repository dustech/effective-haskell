module Main where

-- simple if expression
printSmallNumber num =
  if num < 10
    then print num
    else print "the number is too big!"

-- main = printSmallNumber 3

-- you have to call 'show' to 'num'
-- that's because the if expression must always return
-- the same type

printSmallNumber' num =
  let msg =
        if num < 10
          then show num
          else "the number is too big!"
   in print msg

-- main = printSmallNumber' 3

-- This can get tedious and difficult to read as you add more branches.
-- Enter guard clauses.
guardSize num
  | num < 3 = "that's a small number"
  | num < 10 = "that's a medium number"
  | num < 100 = "that's a pretty big number"
  | num < 1000 = "wow, that's a giant number"
  | otherwise = "that's an unfathomably big number"

joinStringsWithLF s s' = s <> "\n" <> s'

-- main = putStrLn $ joinStringsWithLF (guardSize 3) (guardSize 11)

-- You can use let expressions and where clauses with guards just like with other
-- functions. Anything that you define in a let expression will only be in scope
-- for the particular branch where it’s defined. On the other hand, variables
-- defined inside of a where clause will be in scope for all of the branches
-- of your function

guardSize' num
  | num > 0 =
      let size = "positive"
       in exclaim size
  | num < 3 = exclaim "small"
  | num < 100 = exclaim "medium"
  | otherwise = exclaim "large"
  where
    exclaim message = "that's a " <> message <> " number!"

main = putStrLn $ joinStringsWithLF (guardSize' 3) (guardSize' 11)