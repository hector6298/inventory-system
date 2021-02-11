module Worker where
import System.IO
import System.Directory
import Control.Monad
import Data.Maybe
import Types
import Data.List
import Control.Exception
import Utils (saveNew)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.DeepSeq
import Control.Exception

{-# LANGUAGE FlexibleContexts #-}
loadWorkers :: FilePath -> IO [Worker]
loadWorkers path = do
  file <- readFile path
  evaluate (force file)
  let list' = [a | a <- map read (lines file) :: [Worker]]
  return list'

findWorker :: [Worker] -> Int -> IO (Maybe Worker)
findWorker workers n = do
  let worker = find (\x -> workerId x == n) workers
  return worker

findWorkerByName :: String -> IO (Maybe Worker)
findWorkerByName name = do
  workers <- loadWorkers workers_db
  let worker = find (\x -> workerName x == name) workers
  return worker

removeWorker :: Int -> IO ()
removeWorker number = do 
  contents <- readFile workers_db
  let oldWorkerList = lines contents
      numberedStock = zipWith (\n line -> show n ++ " - " ++ line) [0..] oldWorkerList
  
  let newWorkerList = unlines $ delete (oldWorkerList !! number) oldWorkerList
  bracketOnError (openTempFile "db" "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newWorkerList
      hClose tempHandle
      removeFile "db/workers_db"
      renameFile tempName "db/workers_db")

updateWorker :: [Worker] -> Int -> Int -> IO (Maybe Worker)
updateWorker workers workerId orderId = do
  worker <- findWorker workers workerId
  removeWorker (workerId-1 )
  if isNothing worker 
    then return Nothing
    else return (Just Worker{workerId= workerId, workerName= workerName (fromJust worker), availability=Busy orderId})
-- Chequear 

maybeUpdateWorkerAndSave :: [Worker] -> Int -> Int -> FilePath -> IO ()
maybeUpdateWorkerAndSave workers workerId orderId path = do
    worker <- updateWorker workers workerId orderId
    if isNothing worker
      then putStrLn "Cannot Save, as no value is present"
      else do 
          saveNew (fromJust worker) path
          putStrLn $ show worker

addWorker :: IO Worker
addWorker = do
  putStrLn "Insert worker name"
  name <- getLine 
  workers <- loadWorkers workers_db
  let id = (+1) $ workerId $ last workers
  return Worker{workerId=id, workerName=name, availability=Available}

printWorkers:: [Worker] -> IO ()
printWorkers workerList = do 
  putStrLn "Worker Id | Worker Name | Availability"
  putStrLn "_____________________________________________________"
  prettyPrintedStock <- forM workerList (\worker -> do
      return $ show (workerId worker) ++ "\t    |  " ++ show (workerName worker) ++ "| " ++ show (availability worker))
  mapM_ putStrLn prettyPrintedStock

addWorkerAndSave= do
  stock <- addWorker
  _ <- saveNew  stock workers_db
  putStrLn $ show stock

updateWorkerMain = do
  workers <- loadWorkers workers_db 
  printWorkers workers
  putStrLn "Insert the worker id of the worker you want to update"
  workerId <- getLine 
  putStrLn "Insert order id"
  orderId <- getLine
  maybeUpdateWorkerAndSave workers (read workerId) (read orderId) workers_db

removeWorkerMain = do
  workers <- loadWorkers workers_db 
  printWorkers workers
  putStrLn "Insert the worker id of the worker you want to remove"
  workerId <- getLine 
  removeWorker ((read workerId) - 1)
