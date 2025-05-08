module Main where


makeAndReadFile:: Int -> IO String
makeAndReadFile fnumber =
    let fname = "/tmp/test/" <> show fnumber
    in writeFile fname fname >> readFile fname
  
unsafe:: IO ()
unsafe =
    let files = mapM makeAndReadFile[1..50000]:: IO [ String ]
    in files >>= (putStrLn . show)


makeAndShow :: Int -> IO ()
makeAndShow n =
    makeAndReadFile n >>= putStrLn

-- You might at first think about making a very simple modification to our unsafe function to use this new function:
-- ​ 	safe :: ​IO​ ()
-- ​ 	safe =
-- ​ 	  mapM makeAndShow [1..500]

-- If you try to build that, you’ll realize it doesn’t quite work, because mapM wants to give us a list of values.
--  Since all of our values are just () there’s not much sense in that. We could change the type of our function to IO [()]
--   but that’s a bit of a code smell. Let’s see if we can do better. 
--     We know that >> lets us sequence IO actions without caring about the value, 
--     and we can use foldl to reduce a list of values.
--      We can put those two ideas together to reduce a list down to a single IO action:

safe = 
    foldl (\io id -> 
        io >> makeAndShow id
        ) (return ()) [1..50000]

-- We start our fold with an empty IO action containing an initial value of ().
--  Then we reduce our list with >>, each time sequencing the previous IO action with the current one, 
--  and discarding the results. In the end, we’re left with a single IO action.

-- This pattern turns out to be a very common one, and there’s a built-in function for it, called mapM_.
--  We can use it to implement the final version of our function:


safe' = mapM_ makeAndShow [1..50000]


main = safe'


