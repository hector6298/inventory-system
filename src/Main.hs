module Main where
import System.IO
import System.Exit
import Control.Monad
import Account
import Stock
import Order
import Types
import Worker
import Utils
import Asset

main :: IO ()
main = do
  putStrLn "----------Welcome to Inventory System----------"
  credentials <- login
  accounts <- loadAccounts db_path
  if authenticate credentials accounts
    then do
      putStrLn "You was successfully logged in"
      processMenu
    else do putStrLn "Username or password does not match"

login :: IO (UserName, Password)
login = do
  putStrLn "Please enter your username: "
  username <- getLine
  putStrLn "Password: "
  password <- getLine
  return (username, password)

menu = do
  putStrLn "Make your choice: "
  putStrLn "1. Read stock file"
  putStrLn "2. Add unit to stock"
  putStrLn "3. Remove unit from stock"
  putStrLn "4. Sort stock"
  putStrLn "5. Merge file to stock"
  putStrLn "6. Duplicate unit"
  putStrLn "7. Find unit"
  putStrLn "8. Read orders file"
  putStrLn "9. Print receipt"
  putStrLn "10. Create worker"
  putStrLn "11. Read worker file"
  putStrLn "12. Update worker"
  putStrLn "13. Remove worker"
  putStrLn "14. Place order"
  putStrLn "15. Create asset"
  putStrLn "16. Read asset file"
  putStrLn "17. Update asset"
  putStrLn "18. Remove asset"
  putStrLn "0. Exit"

processMenu = do
  menu
  line' <- getLine
  let choice = read line' :: Int
  case choice of 1 -> printStock $ loadStock stock_db 
                 2 -> addUnitAndSave
                 3 -> removeUnit
                 4 -> stockSorting
                 5 -> doconcat
                 6 -> doreplicate
                 7 -> printUnitFound
                 8 -> printOrdersHelper
                 9 -> printReceipt
                 10 -> addWorkerAndSave
                 11 -> do
                    workers <-loadWorkers workers_db
                    printWorkers workers
                 12 -> updateWorkerMain 
                 13 -> removeWorkerMain
                 14 -> maybeAddAndSave placeOrder  orders_db 
                 15 -> addAssetAndSave
                 16 -> do
                    assets <-loadAssets assets_db
                    printAssets assets
                 17 -> updateAssetMain
                 18 -> removeAssetMain
                 0 -> exitSuccess
                 otherwise -> putStrLn "not exists"
  processMenu
