module Main where

main = print ""

-- More often than not, when you see data types that hold a single function,
--  they’ll be records with a field that gives the function some useful name:

data StringParser = StringParser {runStringParser :: String -> (String, String)}

-- We’ll start by writing a function called takeCharacters that will take
--  some number of characters off the front of a string. We don’t just want
--   to return those characters though, we also want to return the remainder
--   of the string that we haven’t dealt with yet, so we’ll return a tuple.
--    The first element of the tuple will be the data we just parsed,
--    and the second element will be the data we haven’t yet parsed

takeCharacters numCharacters inputString =
  splitAt numCharacters inputString

takeCharacters' numCharacters = stringParser
  where
    stringParser :: String -> (String, String)
    stringParser = \inputString ->
      splitAt numCharacters inputString

-- It’s a small change to go from this refactored version of takeCharacters
-- to a full StringParser version. We can create a StringParser value from
-- a function with the type String -> (String,String) and that happens to
-- be exactly the type of stringParser function we’ve already defined.
-- We just need to pass the stringParser function into the StringParser
-- value constructor

takeCharacters'' numCharacters = StringParser $
  \inputString -> splitAt numCharacters inputString

getNextWord = StringParser $ \someString ->
  case break (== ' ') someString of
    (nextWord, "") -> (nextWord, "")
    (nextWord, rest) -> (nextWord, tail rest)

-- we can write a function that runs two parsers, one after another

combineParsers firstParser secondParser = StringParser $ \someString ->
  let (_firstPart, firstResult) = runStringParser firstParser someString
   in runStringParser secondParser firstResult

getNextWordAfterTenLetters :: StringParser
getNextWordAfterTenLetters =
  combineParsers (takeCharacters'' 10) getNextWord

tenLettersAfterTheFirstWord :: StringParser
tenLettersAfterTheFirstWord =
  combineParsers getNextWord (takeCharacters'' 10)

-- In all of these examples, the first string of our tuple is the one that
-- we’re really interested in. The second string is just holding the rest
-- of the data in case we want to do something else with it, like use it
-- with a different parser. When In all of these examples, the first string
-- of our tuple is the one that we’re really interested in.
-- The second string is just holding the rest of the data in case
-- we want to do something else with it, like use it with a different parser.
--  When we’re building things like this, we’ll usually make a helper
--   function that will run a parser for us and just give us the value
--    we’re interested in:we’re building things like this, we’ll usually
--    make a helper function that will run a parser for us and just give
--     us the value we’re interested in:

parseString parser inputString =
  fst $ runStringParser parser inputString
