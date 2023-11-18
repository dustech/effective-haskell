{-# LANGUAGE RecordWildCards #-}

module Main where

-- You can reduce some of this boilerplate by enabling a language extension
-- to add a new feature to Haskell

-- let’s enable the RecordWildCards extension

-- To use a language pragma to enable the RecordWildCards extension,
-- add this to the top of your source file

--  You can also enable language extensions in ghci with :set or :seti.
--   When you are enabling a language extension from ghci
--   (or on the command line) prefix the extension name with -X,
--   for example:

--  	λ :seti -XRecordWildCards

-- Using :set will enable the extension for both code that you are
-- typing directly into ghci as well as new code that you load;
-- :seti will only apply the extension to the code that you type
-- into ghci. In general, it’s best to use :seti for enabling
-- language extensions. If you use :set you might forget to include
-- a language extension needed to compile a file in the file itself,
-- and then you’ll find that your program fails to compile even though
--  you can load it interactively.

data CustomerInfo = CustomerInfo
  { firstName :: String,
    lastName :: String,
    widgetCount :: Int,
    balance :: Int
  }

-- customerFactory fname lname =
--   CustomerInfo
--     { balance = 0,
--       widgetCount = 5,
--       firstName = fname,
--       lastName = lname
--     }

-- Now instead of pattern matching out all of the fields, we’ll use the record
-- wildcards to automatically bring all of the fields into scope:

showCustomer CustomerInfo {..} =
  firstName
    <> " "
    <> lastName
    <> " "
    <> show widgetCount
    <> " "
    <> show balance

-- You can see in this example that we’re bringing into scope all the fields
-- of our record as variables that we can use however we like. We can also
-- use record wildcards to create a new value. In that case, we still need
-- to use bindings to define our variables with names that match the record
-- fields, but by using record wildcards we can forgo explicitly setting
-- each field.

-- Let’s refactor our customerGeorge function to use record wildcards to create
-- the value that we’re returning

customerGeorge =
  let firstName = "George"
      lastName = "Bird"
      widgetCount = 10
      balance = 100
   in CustomerInfo {..}

-- The field names don’t necessarily need to be defined as let bindings;
--  you can also use names bound to function parameters in wildcards.
--  As an example, let’s refactor our customer factory function:

customerFactory firstName lastName =
  let widgetCount = 5
      balance = 0
   in CustomerInfo {..}

main = print $ showCustomer customerGeorge

-- Naming Record Fields

-- The choice of how to name your record fields can have a big impact on the
-- way that you write your code. As you just learned, choosing overly generic
-- names can lead you to refactoring the structure of your source code
-- so that you can use qualified imports to disambiguate names.
-- Another way to disambiguate names is to choose different names
-- for your record fields. A common idiom in Haskell applications
-- is to simply prefix the record field name with the name of the type,
-- so for example, your CustomerInfo record would become:

-- data​ ​CustomerInfo​ = ​CustomerInfo​
-- ​ 	    { customerInfoFirstName   :: ​String​
-- ​ 	    , customerInfoLastName    :: ​String​
-- ​ 	    , customerInfoWidgetCount :: ​Int​
-- ​ 	    , customerInfoBalance     :: ​Int​
-- ​ 	    }