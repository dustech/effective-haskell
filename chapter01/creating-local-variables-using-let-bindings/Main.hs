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

-- main = putStrLn $ extendedGreeting' "George"

-- When you create a let binding, the expression you are binding a name to doesn’t
-- need to be a constant, like a string or a number. You can also use let bindings
-- to define new functions.

extendedGreeting'' person =
  let joinWithNewlines a b = a <> "\n" <> b
      hello = makeGreeting "Hello" person
      goodbye = makeGreeting "Goodbye" person
   in joinWithNewlines hello goodbye

-- main = putStrLn $ extendedGreeting'' "George"

-- Haskell supports recursive let bindings, which means that the items inside of
--   our let bindings can refer to one another. The order doesn’t matter

extendedGreeting''' person =
  let joinWithNewlines a b = a <> "\n" <> b
      joined = joinWithNewlines hello goodbye
      hello = makeGreeting "Hello" person
      goodbye = makeGreeting "Goodbye" person
   in joined

-- main = putStrLn $ extendedGreeting''' "George"

-- Let bindings can also be nested. For example, if you are defining a new function
--    inside of a let expression, and you want to define some variables inside of
--     that function, you can use nested let expressions.

extendedGreetingNested person =
  let joinWithNewlines a b = a <> "\n" <> b
      helloAndGoodbye hello goodbye =
        let hello' = makeGreeting hello person
            goodbye' = makeGreeting goodbye person
         in joinWithNewlines hello' goodbye'
   in helloAndGoodbye "Hello Nested" "Goodbye"

-- main = putStrLn $ extendedGreetingNested "George"

-- There’s one final type of binding called a where binding. A where binding
-- follows all the same rules as a let binding, except it comes at the end of
--   a function instead of the beginning, and uses the where keyword instead
--   of let .. in. Any parameters that you’ve bound to a variable name in your
--   function will be available to your where binding, but not anything you’ve
--   defined in a let binding. Conversely, anything you define inside of a where
--     binding will be available to use in let bindings

letWhereGreeting name place =
  let salutation = "Hello " <> name
      meetingInfo = location "Tuesday"
   in salutation <> " " <> meetingInfo
  where
    location day = "we met at " <> place <> " on a " <> day

-- main = putStrLn $ letWhereGreeting "George" "Room"

-- Try rewriting your extendedGreeting function to use a
-- where binding instead of a let binding

extendedGreetingWhere person =
  helloAndGoodbye "Hello" "Goodbye"
  where
    helloAndGoodbye hello goodbye =
      joinWithNewlines hello' goodbye'
      where
        hello' = makeGreeting hello person
        goodbye' = makeGreeting goodbye person
    joinWithNewlines a b = a <> "\n" <> b

main = putStrLn $ extendedGreetingWhere "Carl"

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