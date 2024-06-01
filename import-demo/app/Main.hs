module Main where

import Data.Char
import Data.Text


countNonPrintableCharacters :: [Char] -> Int
countNonPrintableCharacters =
    Prelude.length . Prelude.filter (not . isPrint)

countNonPrintableCharactersInText :: Text -> Int
countNonPrintableCharactersInText =
    Data.Text.length . Data.Text.filter (not . isPrint)


countNonPrintableCharactersStringAndText :: [Char] -> (Int, Int)
countNonPrintableCharactersStringAndText input =
     ( countNonPrintableCharacters input
       , countNonPrintableCharactersInText $ pack input)

candidateString :: String
candidateString = "\v\t\aHello count!\n\n\t"

        
main :: IO ()
main = do
    let counted = show $ countNonPrintableCharactersStringAndText candidateString
        msg = "Counted " <> counted <> " in string '" <> candidateString <> "'"
    print msg

    