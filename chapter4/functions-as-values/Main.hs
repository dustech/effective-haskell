module Main where

main = print ""

-- More often than not, when you see data types that hold a single function,
--  they’ll be records with a field that gives the function some useful name:

data StringParser = StringParser {runStringParser :: String -> (String, String)}