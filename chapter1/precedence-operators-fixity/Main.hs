-- passing an argument to a function always has higher precedence than passing
-- that argument to an operator.

-- in this case (show 1) has precedence above the (<>) operator
operatorPrecedenceDemo1 =
  "the sum of "
    <> show 1
    <> " and "
    <> show 2
    <> " is "
    <> show 3

-- this will not compile
-- show 1 + 2
-- because show 1 convert to string and the try to sum to 2

-- anothe example
-- add add 1 2 add 3 4
-- haskell work right-to-left
-- ((((((add add) 1) 2) add) 3) 4)
-- to fix and to run make explicit
-- add (add 1 2) (add 3 4)

-- The choice of whether to parse things left-to-right or right-to-left
-- is known as its associativity

-- Normal function application in Haskell is left associative

--  A fixity declaration has three parts. First, you declare the operator’s associativity:

--     infixl for left associativity
--     infixr for right associativity
--     infix if the operator is not associative

-- to check in GHCi
-- :info (+)

-- custom operator example
-- -- a +++ b = a + b; ​infixl​ 6 +++
--  	​λ​ 3 * 2 +++ 1
-- ​ 	7
-- ​ 	​λ​ 1 +++ 2 * 3
-- ​ 	7
-- -- a +++ b = a + b; ​infixl​ 8 +++
--  	​λ​ 3 * 2 +++ 1
-- ​ 	9
-- ​ 	​λ​ 1 +++ 2 * 3
-- ​ 	9

-- without associativity
-- infix​ 4 ==
