module Main where

makeGreeting salutation person =
  salutation <> " " <> person

-- Next, update your makeGreeting method to create an
-- intermediate value using a let binding.

-- main = print $ makeGreeting "Hello" "George"

makeGreeting' salutation person =
  let messageWithTrailingSpace = salutation <> " "
   in messageWithTrailingSpace <> person

-- main = print $ makeGreeting' "Hello" "George"

-- You aren’t limited to a single variable inside of a let binding;
-- you can create as many different local variables as you want.

extendedGreeting person =
  let hello = makeGreeting "Hello" person
      goodDay = makeGreeting "I hope you have a nice afternoon" person
      goodBye = makeGreeting "See you later" person
   in hello <> "\n" <> goodDay <> "\n" <> goodBye

main = print $ extendedGreeting "George"