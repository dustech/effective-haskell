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

-- main = putStrLn $ extendedGreeting "George"

-- In this example, hello references the helloStr variable that we don’t define
-- until later in the same let binding.

extendedGreeting' person =
  let hello = makeGreeting helloStr person
      goodDay = makeGreeting "I hope you have a nice afternoon" person
      goodBye = makeGreeting "See you later" person
      helloStr = "Hello"
   in hello <> "\n" <> goodDay <> "\n" <> goodBye

main = putStrLn $ extendedGreeting' "George"

-- In Haskell, show, print, and putStrLn are three commonly used functions
-- for output, but they serve slightly different purposes and behave in
-- different ways:

--   show:
--       Type: Show a => a -> String
--       Purpose: show takes a value of any type that is an instance of the Show
--       type class and converts it to a string. It doesn't print anything to the
--       screen. Instead, it produces a string representation of the value, which
--       can then be printed using other functions.
--       Example: show 123 produces the string "123".
--       Usage: It's useful when you want to convert values into a string
--       representation, for example, for concatenating with other strings
--       or for later printing.

--   putStrLn:
--       Type: String -> IO ()
--       Purpose: putStrLn takes a string and prints it to the console, followed
--       by a new line. It's an IO function, meaning it produces an action which,
--       when executed, will have a side effect
--       (in this case, printing to the screen).
--       Example: putStrLn "Hello, world!" prints "Hello, world!" followed
--       by a new line.
--       Usage: Used for printing strings to the console.

--   print:
--       Type: Show a => a -> IO ()
--       Purpose: print is somewhat a combination of show and putStrLn. It takes
--       a value of any type that is an instance of Show, converts it to a string
--         using show, and then prints that string to the console followed
--         by a new line. It's a convenient way to print values directly.
--       Example: print 123 prints 123 to the console.
--       Usage: Used when you want to print the string representation of a value
--       directly to the console.