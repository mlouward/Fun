{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_grains (
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
version = Version [1,2,0,6] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Maxime\\OneDrive - De Vinci\\ESILV A4\\haskell\\grains\\.stack-work\\install\\633f0ad7\\bin"
libdir     = "C:\\Users\\Maxime\\OneDrive - De Vinci\\ESILV A4\\haskell\\grains\\.stack-work\\install\\633f0ad7\\lib\\x86_64-windows-ghc-8.8.3\\grains-1.2.0.6-EZCQRZNd2Rn8MkFOQ4RaqC-test"
dynlibdir  = "C:\\Users\\Maxime\\OneDrive - De Vinci\\ESILV A4\\haskell\\grains\\.stack-work\\install\\633f0ad7\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\Maxime\\OneDrive - De Vinci\\ESILV A4\\haskell\\grains\\.stack-work\\install\\633f0ad7\\share\\x86_64-windows-ghc-8.8.3\\grains-1.2.0.6"
libexecdir = "C:\\Users\\Maxime\\OneDrive - De Vinci\\ESILV A4\\haskell\\grains\\.stack-work\\install\\633f0ad7\\libexec\\x86_64-windows-ghc-8.8.3\\grains-1.2.0.6"
sysconfdir = "C:\\Users\\Maxime\\OneDrive - De Vinci\\ESILV A4\\haskell\\grains\\.stack-work\\install\\633f0ad7\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "grains_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "grains_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "grains_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "grains_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "grains_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "grains_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
