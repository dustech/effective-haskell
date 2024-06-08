module Main where

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

-- main = print ""

getPersonBalance person =
  case person of
    Customer customerInfo ->
      Just $ customerBalance customerInfo
    _ ->
      Nothing

getPersonSalary person =
  case person of
    Employee employeeInfo ->
      Just $ employeeSalary employeeInfo
    _ ->
      Nothing

getPersonManager person =
  case person of
    Employee employeeInfo ->
      Just $ employeeManagerName employeeInfo
    _ ->
      Nothing

customerGeorge =
  Customer $
    CustomerInfo
      { customerName = "George",
        customerBalance = 200_000
      }

employeePorter =
  Employee $
    EmployeeInfo
      { employeeName = "Porter",
        employeeManagerName = "Nani",
        employeeSalary = 150_000
      }

employeeJane =
  Employee $
    EmployeeInfo
      { employeeName = "Jane",
        employeeManagerName = "Nani",
        employeeSalary = 355_000
      }

totalEmployeesSalary :: [Person] -> Int
totalEmployeesSalary =
  let getSalFor = getSal . getPersonSalary
      getSal s =
        case s of
          Just s -> s
          Nothing -> 0
   in foldr ((+) . getSalFor) 0

myPeople = [customerGeorge, employeePorter, employeeJane]

main = print $ totalEmployeesSalary myPeople

-- Although useful, sometimes we want a slightly richer choice between two types,
-- especially when we might want to represent success or detailed failure, or a
-- choice between two paths. In this case, you can use another common type defined
-- in Prelude, the Either type. Either takes two type parameters, representing the
-- types of a left and right value. By convention in Haskell applications, Left
-- typically represents an error case, and Right represents a success case.

-- data Either a b = Left a | Right b

eitherToMaybe e =
  case e of
    Left _ -> Nothing
    Right val -> Just val

handleMissingRight e =
  case e of
    Left err -> Left err
    Right (Just val) -> Right val
    Right Nothing -> Left "Missing value"

myError = Left "Error!"

myRightJust = Right $ Just "Right!"

myRightNothing = Right Nothing

-- resMyError = show $ handleMissingRight myError