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