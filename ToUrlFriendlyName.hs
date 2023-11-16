module ToUrlFriendlyName where

import Data.Text (pack, replace, toLower, unpack)
import System.Environment (getArgs)

-- replaceSpaceWithDash :: [char] -> [char]
replaceSpaceWithDash =
  replace (pack " ") (pack "-")

makeUrlFriendlyFor =
  toLower . replaceSpaceWithDash

main = do
  args <- getArgs
  case args of
    [] -> error "[Error] No string provided. Please provide an argument string."
    (arg : _) -> putStrLn $ unpack $ makeUrlFriendlyFor (pack arg)