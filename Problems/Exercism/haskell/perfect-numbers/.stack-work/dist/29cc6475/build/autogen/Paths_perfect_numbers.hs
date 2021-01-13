{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_perfect_numbers (
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
version = Version [1,1,0,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\perfect-numbers\\.stack-work\\install\\cc798f25\\bin"
libdir     = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\perfect-numbers\\.stack-work\\install\\cc798f25\\lib\\x86_64-windows-ghc-8.8.3\\perfect-numbers-1.1.0.4-IKO9IbaB7sp84W87q26zYj"
dynlibdir  = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\perfect-numbers\\.stack-work\\install\\cc798f25\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\perfect-numbers\\.stack-work\\install\\cc798f25\\share\\x86_64-windows-ghc-8.8.3\\perfect-numbers-1.1.0.4"
libexecdir = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\perfect-numbers\\.stack-work\\install\\cc798f25\\libexec\\x86_64-windows-ghc-8.8.3\\perfect-numbers-1.1.0.4"
sysconfdir = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\perfect-numbers\\.stack-work\\install\\cc798f25\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "perfect_numbers_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "perfect_numbers_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "perfect_numbers_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "perfect_numbers_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "perfect_numbers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "perfect_numbers_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
