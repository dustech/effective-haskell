module Main where

main = print ""

-- you can also use record syntax for sum types

-- data Person
--   = Customer
--       { name :: String,
--         balance :: Int
--       }
--   | Employee
--       { name :: String,
--         managerName :: String,
--         salary :: Int
--       }

-- george =
--   Customer
--     { name = "Georgie Bird",
--       balance = 100
--     }

-- porter =
--   Employee
--     { name = "Porter P. Pupper",
--       managerName = "Remi",
--       salary = 10
--     }

-- The risk of runtime errors from these partial record selectors means that most
-- Haskell developers generally avoid directly combining sum types and records.
--  Instead of directly mixing records and sum types, we can create a new record
--  for each constructor. This will prevent us from ever trying to access fields
--  that don’t exist in a particular branch of our sum type

data CustomerInfo = CustomerInfo
  { customerName :: String,
    customerBalance :: Int
  }

data EmployeeInfo = EmployeeInfo
  { employeeName :: String,
    employeeManagerName :: String,
    employeeSalary :: Int
  }

data Person
  = Customer CustomerInfo
  | Employee EmployeeInfo

george =
  Customer $
    CustomerInfo
      { customerName = "Georgie Bird",
        customerBalance = 100
      }

porter =
  Employee $
    EmployeeInfo
      { employeeName = "Porter P. Pupper",
        employeeManagerName = "Remi",
        employeeSalary = 10
      }

-- One way that we can work around this is to add functions to our API to make it
-- easier to get at data that exists for all of the different potential values
-- in our sum type. For example, since both customers and employees have names,
-- we can add a function getPersonName to get the name for any person:

getPersonName person =
  case person of
    Employee employee -> employeeName employee
    Customer customer -> customerName customer

-- getPersonManager person =
--   case person of
--     Employee employee -> employeeManagerName employee
--     Customer customer -> undefined

-- What we need to do is return the name of a manager if the person is an employee,
--    but if they are a customer we need some value that says “nothing to see here,
--      move along.” That sounds a lot like a sum type! Let’s create a new type
--      called MaybeString to capture this

data MaybeString = NoString | SomeString String

data MaybeInt = NoInt | SomeInt Int

getPersonManager person =
  case person of
    Employee employee -> SomeString (employeeManagerName employee)
    Customer _customer -> NoString

-- We can take the same approach with customerBalance and employeeSalary.
-- You can imagine that we could create a new type named MaybeInt that works just
-- like MaybeString, and use it to create functions to get a person’s balance and
-- salary:

getPersonBalance, getPersonSalary :: Person -> MaybeInt
getPersonBalance person =
  case person of
    Employee _employee -> NoInt
    Customer customer -> SomeInt (customerBalance customer)
getPersonSalary person =
  case person of
    Employee employee -> SomeInt (employeeSalary employee)
    Customer customer -> NoInt

-- data​ ​Maybe​ a = ​Nothing​ | ​Just​ a

-- we can avoid all of this additional work by making use of type parameters.
--  Type parameters allow you to make a type constructor that takes some
--  types as parameters. When you pass some parameters to a type constructor,
--  you end up with a type.

-- In this example, Maybe by itself isn’t a full type,
--  so we call it a type constructor.
