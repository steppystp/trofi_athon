{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_trofi (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/970031/.cabal/bin"
libdir     = "/Users/970031/.cabal/lib/aarch64-osx-ghc-8.10.7/trofi-0.1.0.0-inplace-warehouse-log"
dynlibdir  = "/Users/970031/.cabal/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/970031/.cabal/share/aarch64-osx-ghc-8.10.7/trofi-0.1.0.0"
libexecdir = "/Users/970031/.cabal/libexec/aarch64-osx-ghc-8.10.7/trofi-0.1.0.0"
sysconfdir = "/Users/970031/.cabal/etc"

getBinDir     = catchIO (getEnv "trofi_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "trofi_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "trofi_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "trofi_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trofi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trofi_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
