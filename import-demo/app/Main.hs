module Main where

import qualified Data.Char as Char
import Data.Text as T
import Data.Text.Encoding as T

countNonPrintableCharacters :: [Char] -> Int
countNonPrintableCharacters =
    Prelude.length . Prelude.filter (not . Char.isPrint)

countNonPrintableCharactersInText :: Text -> Int
countNonPrintableCharactersInText =
    T.length . T.filter (not . Char.isPrint) . T.decodeUtf8 . T.encodeUtf8


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

    