module Order where
import Control.Monad
import Data.Maybe
import Data.List
import Numeric
import Stock
import Customer

-- Synonyms
type QuantityOrdered = Int

orders_db = "db/orders_db"

type UserName = String

data Order = Order { ordStockCode :: StockCode,
                     customer :: CustomerId,
                     qtyOrdered :: QuantityOrdered,
                     number :: Int
                   } deriving (Show, Read, Ord, Eq)

data CustomerOrders = CustomerOrders {
      customerOrdId :: CustomerId,
      orderHistory:: [Order]
} deriving (Show, Read)

orderUnzip :: (Int, Order) -> Order
orderUnzip tup = assignNumber (fst tup) (snd tup)

assignNumber :: Int -> Order -> Order
assignNumber number order = Order{ordStockCode=ordStockCode order, customer=customer order, qtyOrdered=qtyOrdered order, number=number}

printOrders :: [(Int, Order)] -> IO ()
printOrders input = do
  let orders = map orderUnzip input 
  putStrLn "Num\t |Stock code  |Quantity\t|Price\t\t|Description"
  putStrLn "__________________________________________________________________"
  prettyPrintedOrder <- forM orders (\order -> do
      unit <- fetchOrder order
      let quantity = qtyOrdered order
          unitp = if (qtyOrdered order) > 10 
                  then discountPrice unit
                  else unitPrice unit
          price = (unitp::Double) * (fromIntegral quantity)
      return $ show (number order) ++ "\t |"  ++ show (ordStockCode order) ++ "\t    |" ++ show (qtyOrdered order) ++ "\t\t|$ " ++  (showGFloat (Just 2) price "") ++ "\t|" ++ show (description unit))
  mapM_ putStrLn prettyPrintedOrder
  
fetchOrder :: Order -> IO (Stock)
fetchOrder order = do
  let stock = findByCode $ ordStockCode order
  unitMaybe <- stock
  let unit = fromJust unitMaybe
  return unit

loadOrders :: FilePath -> IO ([(Int, Order)])
loadOrders path = do
  file <- readFile path
  let list' = [a | a <- map read (lines file) :: [Order]]
      numberedList = zip [0..length list'] list'
  return (numberedList)

-- NEW FUNCTIONALITY
placeOrder :: IO ( Maybe Order)
placeOrder = do
  printAllStock
  putStrLn "PLACE ORDER"
  stockFound <- findUnitBy
  order <- loadOrders orders_db
  putStrLn "Insert client"
  clientName <- getLine
  customer <- findCustomerByName clientName
  customer <- if isNothing customer
                 then do 
                  addCustomer 
                  findCustomerByName clientName
                 else
                  findCustomerByName clientName
  
  let ordnum = (+1) $  fst $ last order
  if isNothing stockFound
    then return Nothing 
    else do
      quantity <- getLine
      return $ Just Order{
                        ordStockCode=stockCode (fromJust stockFound), 
                        customer=customerId (fromJust customer), 
                        qtyOrdered=read quantity::Int,
                        number=ordnum
                      }


findOrder :: Int -> IO (Maybe Order)
findOrder n = do
  orders' <- loadOrders orders_db
  let orders = map orderUnzip orders'
  let order = find (\x -> number x == n) orders
  return (order)

printReceipt = do
  putStrLn "Which order to print? Select number"
  orders' <- loadOrders orders_db
  printOrders orders'
  numberString <- getLine
  let number = read numberString
  orderMaybe <- findOrder number
  let order = fromJust orderMaybe
  customerMaybe <- findCustomer $ customer order
  let customer = fromJust customerMaybe
  unit <- fetchOrder order 
  putStrLn "_____________________________________________"
  putStrLn "Customer name:"
  putStrLn $ show $ customerName customer
  putStrLn "Customer address:"
  putStrLn $ show $ customerAddress customer
  putStrLn "Quantity:"
  putStrLn $ show $ qtyOrdered order
  putStrLn "Description:"
  putStrLn $ show $ description unit
  putStrLn "Price______"
  putStrLn $ show $ unitPrice unit
  putStrLn "_______________________________________________"

printOrdersHelper = do
  orders <- loadOrders orders_db
  printOrders orders
