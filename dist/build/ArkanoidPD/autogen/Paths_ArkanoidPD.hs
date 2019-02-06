{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ArkanoidPD (
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

bindir     = "C:\\Users\\mauro\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\mauro\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3\\ArkanoidPD-0.1.0.0-9bQZvDNTBkD6vzUsQ3gVod-ArkanoidPD"
dynlibdir  = "C:\\Users\\mauro\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3"
datadir    = "C:\\Users\\mauro\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3\\ArkanoidPD-0.1.0.0"
libexecdir = "C:\\Users\\mauro\\AppData\\Roaming\\cabal\\ArkanoidPD-0.1.0.0-9bQZvDNTBkD6vzUsQ3gVod-ArkanoidPD\\x86_64-windows-ghc-8.6.3\\ArkanoidPD-0.1.0.0"
sysconfdir = "C:\\Users\\mauro\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ArkanoidPD_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ArkanoidPD_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ArkanoidPD_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ArkanoidPD_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ArkanoidPD_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ArkanoidPD_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
