module Customer where
import Data.List
import Utils (saveNew)
import Types



loadCustomers :: FilePath -> IO [Customer]
loadCustomers path = do
  file <- readFile path
  let list' = [a | a <- map read (lines file) :: [Customer]]
  return (list')

findCustomer :: Int -> IO (Maybe Customer)
findCustomer n = do
  customers <- loadCustomers customers_db
  let customer = find (\x -> customerId x == n) customers
  return (customer)

findCustomerByName :: String -> IO (Maybe Customer)
findCustomerByName name = do
  customers <- loadCustomers customers_db
  let customer = find (\x -> customerName x == name) customers
  return (customer)

addCustomer :: IO Customer
addCustomer = do
  putStrLn "Insert client name"
  name <- getLine 
  putStrLn "Insert client address"
  address <- getLine
  customers <- loadCustomers customers_db
  let id = (+1) $ customerId $ last customers
  return Customer{customerId=id, customerName=name, customerAddress=address}


addCustomerAndSave= do
  stock <- addCustomer
  _ <- saveNew  stock customers_db
  putStrLn $ show stock
