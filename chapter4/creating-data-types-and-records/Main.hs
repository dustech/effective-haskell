module Main where

-- there’s a commonly used type that doesn’t have any inhabitants called
-- Void and its definition is simply

-- data Void

-- Values with a single inhabitant can also be quite useful in practice.
-- The type with a single inhabitant in Haskell is often called Unit, or ().

-- data () = ()

-- This defThis defines the existence of a new type called CustomerInfo.
-- Unfortunately, as we’ve written the code, the only thing it does is define a
-- new type. ines the existence of a new type called CustomerInfo.
-- Unfortunately, as we’ve written the code, the only thing it does is
-- define a new type.

-- data CustomerInfo

-- We’ll start by creating a value constructor.
-- A value constructor is a special function that lets us create a new value
-- of a certain type.

-- add Bool to define whether the user was active or not

-- When we add a single parameter, the number of inhabitants for our new type
-- is equal to the number of inhabitants of the value it contains.

-- Our new type now contains two boolean values, and it has four inhabitants:

-- Because the number of inhabitants of a type increases multiplicatively,
-- we often refer to these types in Haskell as product types.

-- data CustomerInfo = CustomerInfo Bool Bool

-- someCustomerInfo = CustomerInfo True True

-- Let’s dump our booleans and create a CustomerInfo that holds data for our
-- customer’s first and last name, the number of items they’ve ordered,
-- and their account balance:

data CustomerInfo = CustomerInfo String String Int Int

-- Next, let’s create a useful example customer that we can work with,
--  by applying some values to our value constructor function:

customerGeorge =
  CustomerInfo "George" "Bird" 10 100

-- To pattern match the fields of a type you can use the type data name,
--  followed by the names you want to bind to each type field,
--  or an underscore for fields you want to ignore.

showCustomer (CustomerInfo first last count balance) =
  let fullName = first <> " " <> last
      name = "name: " <> fullName
      count' = "count: " <> show count
      balance' = "balance: " <> show balance
   in name <> " " <> count' <> " " <> balance'

-- You can also match values in specific fields. For example, let’s write a
-- function that applies a discount to certain customers based on their
-- first and last name
applyDiscount customer =
  case customer of
    (CustomerInfo "George" "Bird" count balance) ->
      CustomerInfo "George" "Bird" count (balance `div` 4)
    (CustomerInfo "Porter" "Pupper" count balance) ->
      CustomerInfo "Porter" "Pupper" count (balance `div` 2)
    otherCustomer -> otherCustomer

-- This approach to pattern matching out fields works well for small data types
--  where you will generally want access to all or most fields, but as you can
--   imagine it can become cumbersome for larger types or cases where you
--   frequently only want to access a single field. You can work around this
--   by writing a function to access each field of your value

-- firstName (CustomerInfo name _ _ _) = name

-- lastName (CustomerInfo _ name _ _) = name

-- widgetCount (CustomerInfo _ _ count _) = count

-- balance (CustomerInfo _ _ _ balance) = balance

updateFirstName (CustomerInfo _ lastName count balance) firstName =
  CustomerInfo firstName lastName count balance

main = print $ showCustomer $ applyDiscount customerGeorge

data CustomerInfoRecord = CustomerInfoRecord
  { firstName :: String,
    lastName :: String,
    widgetCount :: Int,
    balance :: Int
  }

customerGeorgeRecord =
  CustomerInfoRecord
    { balance = 100,
      lastName = "Bird",
      firstName = "George",
      widgetCount = 10
    }

-- A disadvantage to constructing records this way is you can’t
-- partially apply fields to the data constructor using record syntax.
--  For example, if we wanted to create some function to initialize
--   new customers with some bonus items using record syntax,
--   we would need to manually accept the missing fields as parameters

customerFactory fname lname =
  CustomerInfoRecord
    { balance = 0,
      widgetCount = 5,
      firstName = fname,
      lastName = lname
    }

showCustomerRecord c =
  let fullName = firstName c <> " " <> lastName c
      name = "name: " <> fullName
      count' = "count: " <> show (widgetCount c)
      balance' = "balance: " <> show (balance c)
   in name <> " " <> count' <> " " <> balance'

-- In record syntax you create a product type, but each field of the datatype
--  is assigned a name as well as a type. The names are used to automatically
--   generate functions to get fields from the record. These functions
--   are called field selectors, or often just selectors.
--   There is also special syntax to generate a record with new fields.
--   Let’s rewrite our CustomerInfo type using record syntax:

-- Updating records can also be done easily using record update syntax.

emptyCart customer =
  customer
    { widgetCount = 0,
      balance = 0
    }
