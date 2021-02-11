module Worker where
import System.IO
import Control.Monad(liftM)
import Data.Maybe
import Types
import Data.List
import Utils (saveNew)

loadWorkers :: FilePath -> IO [Worker]
loadWorkers path = do
  file <- readFile path
  let list' = [a | a <- map read (lines file) :: [Worker]]
  return (list')

findWorker :: Int -> IO (Maybe Worker)
findWorker n = do
  workers <- loadWorkers workers_db
  let worker = find (\x -> workerId x == n) workers
  return (worker)

findWorkerByName :: String -> IO (Maybe Worker)
findWorkerByName name = do
  workers <- loadWorkers workers_db
  let worker = find (\x -> workerName x == name) workers
  return (worker)

removeWorker :: Int -> IO ()
removeWorker number = do 
  contents <- readFile workers_db
  let oldStockList = lines contents
      numberedStock = zipWith (\n line -> show n ++ " - " ++ line) [0..] oldStockList
  
  newWorkerList = unlines $ delete (oldWorkerList !! number) oldStockList
  bracketOnError (openTempFile "db" "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newStockList
      hClose tempHandle
      removeFile "db/workers_db"
      renameFile tempName "db/workers_db")

updateWorker :: Int -> Int -> IO (Maybe Worker)
updateWorker workerId orderId = do
  worker <- findWorker workerId
  removeWorker workerId
  if isNothing worker 
    then return Nothing
    else return Worker{workerId= workerId, workerName= workerName, availability=Show Busy orderId}
-- Chequear 

maybeUpdateWorkerAndSave :: Show Worker => Int -> Int -> FilePath -> IO ()
maybeUpdateWorkerAndSave workerId orderId path = do
    item <- updateWorker workerId orderId
    if isNothing item
      then putStrLn "Cannot Save, as no value is present"
      else do 
          saveNew item path
          putStrLn $ show item

addWorker :: IO Worker
addWorker = do
  putStrLn "Insert worker name"
  name <- getLine 
  workers <- loadWorkers workers_db
  let id = (+1) $ workerId $ last workers
  return Worker{workerId=id, workerName=name, availability=Available}

printWorkers:: IO ([Stock]) -> IO ()
printworker input = do
  workerList <- input  
  putStrLn "Worker Id | Worker Name | Availability"
  putStrLn "_____________________________________________________"
  prettyPrintedStock <- forM workerList (\worker -> do
      return $ show (workerId worker) ++ "\t    |$ " ++ show (workerName worker) ++ "|$ " ++ show (availability worker)
  mapM_ putStrLn prettyPrintedStock

addWorkerAndSave= do
  stock <- addWorker
  _ <- saveNew  stock workers_db
  putStrLn $ show stock

updateWorkerMain = do
  printWorkers $ loadWorkers workers_db
  putStrLn "Insert the worker id of the worker you want to update"
  workerId <- getLine 
  putStrLn "Insert order id"
  orderId <- getLine
  maybeUpdateWorkerAndSave workerId orderId workers_db

removeWorkerMain = do
  printWorkers $ loadWorkers workers_db
  putStrLn "Insert the worker id of the worker you want to update"
  workerId <- getLine 
  removeWorker workerId
