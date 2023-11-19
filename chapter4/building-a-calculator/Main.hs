module Main where

import Text.Read (readEither)

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

parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err -> Left err
    Right (e, []) -> Right e
    Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)

--  The words function takes an input string and splits it up along
--   blank space boundaries, returning a list of strings.
--   The unwords function, intuitively, does the opposite, taking a list
--   of strings and joining them all with spaces.

parse' [] = Left "unexpected end of expression"
parse' (token : rest) =
  case token of
    "+" -> parseBinary Add rest
    "*" -> parseBinary Mul rest
    "-" -> parseBinary Sub rest
    "/" -> parseBinary Div rest
    lit ->
      case readEither lit of
        Left err -> Left $ err
        Right lit' -> Right (Lit lit', rest)

-- You might wonder why we’re returning a tuple of an expression and
-- a list of strings here. This is a common pattern when implementing
-- recursive parsers. The idea is that each recursive call will
-- consume some part of the input, and will return the remainder
-- of the input, allowing the caller to make forward progress
-- through the list of tokens.

parseBinary exprConstructor args =
  case parse' args of
    Left err -> Left err
    Right (firstArg, rest') ->
      case parse' rest' of
        Left err -> Left err
        Right (secondArg, rest'') ->
          Right $ (exprConstructor firstArg secondArg, rest'')

run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let answer = show $ eval expr'
       in "The answer is: " <> answer