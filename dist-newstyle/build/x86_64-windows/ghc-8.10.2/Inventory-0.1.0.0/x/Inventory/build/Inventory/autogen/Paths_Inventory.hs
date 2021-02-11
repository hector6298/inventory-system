{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Inventory (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\COMPUTADOR\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\COMPUTADOR\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\Inventory-0.1.0.0-inplace-Inventory"
dynlibdir  = "C:\\Users\\COMPUTADOR\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2"
datadir    = "C:\\Users\\COMPUTADOR\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\Inventory-0.1.0.0"
libexecdir = "C:\\Users\\COMPUTADOR\\AppData\\Roaming\\cabal\\Inventory-0.1.0.0-inplace-Inventory\\x86_64-windows-ghc-8.10.2\\Inventory-0.1.0.0"
sysconfdir = "C:\\Users\\COMPUTADOR\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Inventory_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Inventory_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Inventory_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Inventory_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Inventory_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Inventory_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
