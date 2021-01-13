{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hamming (
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
version = Version [2,3,0,10] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\hamming\\.stack-work\\install\\cc798f25\\bin"
libdir     = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\hamming\\.stack-work\\install\\cc798f25\\lib\\x86_64-windows-ghc-8.8.3\\hamming-2.3.0.10-GQC73LR3tv8EGNSO2zot9Y"
dynlibdir  = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\hamming\\.stack-work\\install\\cc798f25\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\hamming\\.stack-work\\install\\cc798f25\\share\\x86_64-windows-ghc-8.8.3\\hamming-2.3.0.10"
libexecdir = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\hamming\\.stack-work\\install\\cc798f25\\libexec\\x86_64-windows-ghc-8.8.3\\hamming-2.3.0.10"
sysconfdir = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\hamming\\.stack-work\\install\\cc798f25\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hamming_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hamming_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hamming_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hamming_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hamming_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hamming_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
