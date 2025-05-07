module Main where


showWithSeq =
    putStrLn "this is just some text"
    >> putStrLn "there are many lines of it"
    >> putStrLn "not one a new function"


main =
    showWithSeq >>
    putStrLn "test 1" >>
    putStrLn "test 2"
