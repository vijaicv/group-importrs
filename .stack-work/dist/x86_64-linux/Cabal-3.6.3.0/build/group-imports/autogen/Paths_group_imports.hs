{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_group_imports (
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
bindir     = "/home/vijaicv/Desktop/group-imports/.stack-work/install/x86_64-linux/4b21f66d3c93928b07eed3656c3468d21f694cfb09b93610197b629055645fd4/9.2.5/bin"
libdir     = "/home/vijaicv/Desktop/group-imports/.stack-work/install/x86_64-linux/4b21f66d3c93928b07eed3656c3468d21f694cfb09b93610197b629055645fd4/9.2.5/lib/x86_64-linux-ghc-9.2.5/group-imports-0.1.0.0-GGzWEUmz5L4D0nfes8JbOl-group-imports"
dynlibdir  = "/home/vijaicv/Desktop/group-imports/.stack-work/install/x86_64-linux/4b21f66d3c93928b07eed3656c3468d21f694cfb09b93610197b629055645fd4/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/vijaicv/Desktop/group-imports/.stack-work/install/x86_64-linux/4b21f66d3c93928b07eed3656c3468d21f694cfb09b93610197b629055645fd4/9.2.5/share/x86_64-linux-ghc-9.2.5/group-imports-0.1.0.0"
libexecdir = "/home/vijaicv/Desktop/group-imports/.stack-work/install/x86_64-linux/4b21f66d3c93928b07eed3656c3468d21f694cfb09b93610197b629055645fd4/9.2.5/libexec/x86_64-linux-ghc-9.2.5/group-imports-0.1.0.0"
sysconfdir = "/home/vijaicv/Desktop/group-imports/.stack-work/install/x86_64-linux/4b21f66d3c93928b07eed3656c3468d21f694cfb09b93610197b629055645fd4/9.2.5/etc"

getBinDir     = catchIO (getEnv "group_imports_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "group_imports_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "group_imports_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "group_imports_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "group_imports_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "group_imports_sysconfdir") (\_ -> return sysconfdir)




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
