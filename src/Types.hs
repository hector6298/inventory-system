module Types where

-- Customer -- 
type CustomerId = Int
type QuantityOrdered = Int
-- stock --
type StockCode = Int
type UnitPrice = Double
type DiscountPrice = Double
type Description = String
-- Account --
type Password = String
type UserName = String
-- Worker --
type WorkerId = Int
type WorkerName = String

data Account = Account {
    username :: UserName,
    password :: Password
    } deriving (Show, Read)

data Customer = Customer {
      customerId :: CustomerId,
      customerName :: String,
      customerAddress :: String
      } deriving (Show, Read)

data Order = Order { ordStockCode :: StockCode,
                     customer :: CustomerId,
                     qtyOrdered :: QuantityOrdered,
                     number :: Int
                   } deriving (Show, Read, Ord, Eq)

data CustomerOrders = CustomerOrders {
      customerOrdId :: CustomerId,
      orderHistory:: [Order]
} deriving (Show, Read)


data Stock = Stock { stockCode :: StockCode,
                   unitPrice :: UnitPrice,
                   discountPrice :: DiscountPrice,
                   description :: Description } deriving (Show, Read, Ord, Eq)

data Worker = Worker {
    workerId :: WorkerId,
    workerName :: WorkerName,
    availability :: Availability Int

} deriving (Show, Read)

data Asset = Asset {
    assetId :: Int,
    assetDescription :: String,
    status :: Condition
} deriving (Show, Read)

data Condition = Perfect | Maintenance | Damaged | Destroyed deriving (Show, Read, Enum, Eq, Ord)
   
data Availability a = Busy a | Available deriving (Read)

instance Show a => Show (Availability a) where
    show (Busy a) =  "Busy " ++ show a
    show Available = "Available"



instance Functor Availability where 
    fmap f (Busy a) = Busy (f a)
    fmap f Available = Available


db_path = "db/accounts_db"
customers_db = "db/customers_db"
orders_db = "db/orders_db"
stock_db = "db/stock_db"
workers_db ="db/workers_db"
assets_db = "db/assets_db"