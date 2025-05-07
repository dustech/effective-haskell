module Main where
import           GHC.Internal.System.Environment (getArgs)
import           Text.Read                       (readMaybe)


sumArgs :: [String] -> Maybe Int
sumArgs strArgs =
    let intArgs = mapM readMaybe strArgs
    in fmap sum intArgs


-- The first approach to come to mind is probably the one that
--  we’ve been using so far throughout this chapter.
--  We can use a lambda function to unwrap our IO value so
--   that we can work with it:

-- main =
--     getArgs
--     >>= \args -> return (sumArgs args)
--     >>= print

-- Whenever you see return being used with >>=
-- it’s a sign that you might want to refactor your code.

-- main = getArgs >>= return . sumArgs >>= print

-- better
-- main = fmap sumArgs getArgs >>= print

-- using <$> operator
main =
    sumArgs <$> getArgs
    >>= print
