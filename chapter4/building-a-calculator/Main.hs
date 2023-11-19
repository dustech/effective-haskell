module Main where

main = print ""

--  	λ run "+ 3 5"
--  	"The answer is: 8"
--  	λ run "/ 16 4"
--  	"The answer is: 4"
--  	λ run "* 2 / 16 4"
--  	"The answer is: 8"
--  	λ run "- 10 + 1 * 2 / 8 4"
--  	"The answer is: 5"

-- data Expr
--   = Lit Int
--   | Add Int Int
--   | Sub Int Int
--   | Mul Int Int
--   | Div Int Int

-- Not only do we have our four binary operations, we also have literal
-- numbers as part of the grammar of our arithmetic expression.
-- Let’s add a new Lit constructor to represent a literal integer,
--  and update the rest of our type to recursively reference sub-expressions

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

-- ust like you did with peano numbers, you can write a recursive
-- function that will take one of these recursive values and compute
-- a number. Instead of incrementing the number, you’ll decide on
--  an operation based on the constructor for the value:

eval :: Expr -> Int
eval expr =
  case expr of
    Lit num -> num
    Add x y -> eval' (+) x y
    Sub x y -> eval' (-) x y
    Mul x y -> eval' (*) x y
    Div x y -> eval' div x y
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
    eval' op arg1 arg2 =
      op (eval arg1) (eval arg2)

-- eval $ Add (Lit 5) (Sub (Lit 10) (Div (Lit 10) (Lit 2)))

-- Let’s create a parser that will allow us to write nice expressions, like
-- we did in the example we used to start this section.

-- parse :: String -> Either String Expr
-- parse str =
--   case parse' (words str) of
--     Left err -> Left err
--     Right (e, []) -> Right e
--     Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)