module Main where

import Data.Char (isPrint)
import Data.Text hiding (length, filter)
import qualified Data.Text as T (length, filter)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

countNonPrintableCharacters :: [Char] -> Int
countNonPrintableCharacters =
    Prelude.length . Prelude.filter (not . isPrint)

countNonPrintableCharactersInText :: Text -> Int
countNonPrintableCharactersInText =
    T.length . T.filter (not . isPrint) . decodeUtf8 . encodeUtf8


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

    