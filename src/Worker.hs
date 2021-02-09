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