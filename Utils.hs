module Utils where
import System.IO
import System.Directory
import Data.Maybe
saveNew :: Show a => a -> FilePath  -> IO ()
saveNew newItem filename= do
  contents <- readFile filename
  let oldStockList = lines contents
      newStockList = unlines $ oldStockList ++ [(show newItem)]
  (tempName, tempHandle) <- openTempFile "db" "temp"
  hPutStr tempHandle newStockList
  hClose tempHandle
  removeFile filename
  renameFile tempName filename

addAndSave :: Show a => IO a -> FilePath -> IO ()
addAndSave f path = do
  item <- f
  saveNew item path
  putStrLn $ show item

maybeAddAndSave :: Show a => IO (Maybe a) -> FilePath -> IO ()
maybeAddAndSave f path = do
    item <- f
    if isNothing item
    then putStrLn "Cannot Save, as no value is present"
    else do 
         saveNew item path
         putStrLn $ show item
