module Main where

import qualified Data.Char as Char (isPrint)
import Data.Text as T ( filter, length, pack, Text)
import Data.Text.Encoding as T ( encodeUtf8, decodeUtf8 )

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

    