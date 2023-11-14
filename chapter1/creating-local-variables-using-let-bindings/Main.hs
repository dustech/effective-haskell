module Main where

makeGreeting salutation person =
  salutation <> " " <> person

-- Next, update your makeGreeting method to create an
-- intermediate value using a let binding.

makeGreeting' salutation person =
  let messageWithTrailingSpace = salutation <> " "
   in messageWithTrailingSpace <> person

main = print $ makeGreeting' "Hello" "George"