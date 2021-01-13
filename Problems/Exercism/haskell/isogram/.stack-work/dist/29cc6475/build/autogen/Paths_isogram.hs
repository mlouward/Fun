{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_isogram (
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
version = Version [1,7,0,8] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\isogram\\.stack-work\\install\\cc798f25\\bin"
libdir     = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\isogram\\.stack-work\\install\\cc798f25\\lib\\x86_64-windows-ghc-8.8.3\\isogram-1.7.0.8-7W2lA31yjnaLfC2ZBxr4bi"
dynlibdir  = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\isogram\\.stack-work\\install\\cc798f25\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\isogram\\.stack-work\\install\\cc798f25\\share\\x86_64-windows-ghc-8.8.3\\isogram-1.7.0.8"
libexecdir = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\isogram\\.stack-work\\install\\cc798f25\\libexec\\x86_64-windows-ghc-8.8.3\\isogram-1.7.0.8"
sysconfdir = "C:\\Users\\maxime.louward\\OneDrive - De Vinci\\ESILV A4\\haskell\\isogram\\.stack-work\\install\\cc798f25\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "isogram_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "isogram_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "isogram_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "isogram_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "isogram_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "isogram_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
