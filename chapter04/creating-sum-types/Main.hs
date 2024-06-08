module Main where

-- algebraic datatypes ADTs

-- Let’s work through a simple example by looking at an implementation of Bool,
-- which is one of the simplest sum types that we can define.

data Bool = True | False

data Direction = North | South | East | West

-- We create a value constructor for each branch as we’ve done for previous
-- sum types, but for each of these value constructors we can add any number
-- of parameters, just like we did for product types.

data PreferredContactMethod
  = Email String
  | TextMessage String
  | Mail String String String Int

emailContact = Email "me@example.com"

textContact = TextMessage "+1 307 555 0100"

mailContact = Mail "1123 S. Road St." "Suite 712" "Examplesville, OH" 98142

--
-- This pattern of combining multiple product types into a single new type
-- is sometimes referred to as a sum of products, although the pattern is
-- so common that in practice most Haskell developers will casually just
-- refer to sum, product, or sum-of-product types as “a type.”

-- When you are working with sum types you can still pattern match
-- function arguments directly, but it’s more common to use a case statement.
-- When you are working with sum types you can still pattern match function
-- arguments directly, but it’s more common to use a case statement.

confirmContact contact =
  case contact of
    Email emailAddress ->
      "Okay, I'll email you at " <> emailAddress
    TextMessage number ->
      "Okay, I'll text you at " <> number
    Mail street1 street2 citystate zip ->
      "Okay, I'll send a letter to\n"
        <> street1
        <> "\n"
        <> street2
        <> "\n"
        <> citystate
        <> " "
        <> show zip

-- In some circumstances, you might not care about all of the fields
-- when pattern matching in sum types. In that case, you can use a pair of
--   empty brackets to represent the parameters to the data constructor
--   without having to type them all out

confirmContact' contact =
  case contact of
    Mail {} -> "Okay, I'll send you a letter!"
    Email {} -> "Okay, I'll email you!"
    TextMessage {} -> "Okay, I'll text you!"

-- we can easily express this function since all of our different values are still
-- of type PreferredContactMethod

contactForUser username =
  case username of
    "George" ->
      Email "george@example.com"
    "Porter" ->
      TextMessage "+1 307 555 0100"
    "Remmy" ->
      Mail "1123 S. Road St." "Suite 712" "Examplesville, OH" 98142
    name ->
      Email $ name <> "@example.com"

confirmContactInfo = confirmContact . contactForUser

--  using sum types to store a list of strings and numbers
data StringOrNumber = S String | N Int

stringsAndNumbers =
  [ S "This list has",
    N 2,
    S "different types of values"
  ]

main = print ""