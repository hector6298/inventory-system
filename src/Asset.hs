module Asset where
import System.IO
import System.Directory
import Control.Monad
import Data.Maybe
import Types
import Data.List
import Utils (saveNew)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.DeepSeq
import Control.Exception

loadAssets :: FilePath -> IO [Asset]
loadAssets path = do
  file <- readFile path
  evaluate (force file)
  let list' = [a | a <- map read (lines file) :: [Asset]]
  return (list')

findAsset :: [Asset] -> Int -> IO (Maybe Asset)
findAsset assets n = do
  let asset = find (\x -> assetId x == n) assets
  return asset

findAssetByName :: String -> IO (Maybe Asset)
findAssetByName name = do
  assets <- loadAssets assets_db
  let asset = find (\x -> assetDescription x == name) assets
  return asset

removeAsset :: Int -> IO ()
removeAsset number = do 
  contents <- readFile assets_db
  let oldAssetList = lines contents
      numberedStock = zipWith (\n line -> show n ++ " - " ++ line) [0..] oldAssetList
  
  let newAssetList = unlines $ delete (oldAssetList !! number) oldAssetList
  bracketOnError (openTempFile "db" "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newAssetList
      hClose tempHandle
      removeFile "db/assets_db"
      renameFile tempName "db/assets_db")

updateAsset :: [Asset] -> Int -> Condition -> IO (Maybe Asset)
updateAsset assets assetId newStatus = do
  asset <- findAsset assets assetId
  removeAsset (assetId-1 )
  if isNothing asset 
    then return Nothing
    else return (Just Asset{assetId= assetId, assetDescription= assetDescription (fromJust asset), status= newStatus})
-- Chequear 

maybeUpdateAssetAndSave :: [Asset] -> Int -> Condition -> FilePath -> IO ()
maybeUpdateAssetAndSave assets assetId status path = do
    asset <- updateAsset assets assetId status
    if isNothing asset
      then putStrLn "Cannot Save, as no value is present"
      else do 
          saveNew (fromJust asset) path
          putStrLn $ show asset

addAsset :: IO Asset
addAsset = do
  putStrLn "Insert asset description"
  name <- getLine 
  assets <- loadAssets assets_db
  let id = (+1) $ assetId $ last assets
  return Asset{assetId=id, assetDescription=name, status=Perfect}

printAssets :: [Asset] -> IO ()
printAssets assetList = do 
  putStrLn "Asset Id | Description | Status"
  putStrLn "_____________________________________________________"
  prettyPrintedStock <- forM assetList (\asset -> do
      return $ show (assetId asset) ++ "\t    | " ++ show (assetDescription asset) ++ "| " ++ show (status asset))
  mapM_ putStrLn prettyPrintedStock

addAssetAndSave= do
  stock <- addAsset
  _ <- saveNew  stock assets_db
  putStrLn $ show stock

updateAssetMain = do
  assets <- loadAssets assets_db 
  printAssets assets
  putStrLn "Insert the asset id of the asset state you want to update"
  assetId <- getLine 
  putStrLn "Insert status: Perfect | Maintenance | Damaged | Destroyed"
  status <- getLine
  maybeUpdateAssetAndSave assets (read assetId) (read status) assets_db

removeAssetMain = do
  assets <- loadAssets assets_db 
  printAssets assets
  putStrLn "Insert the asset id of the asset you want to remove"
  assetId <- getLine 
  removeAsset ((read assetId) - 1)