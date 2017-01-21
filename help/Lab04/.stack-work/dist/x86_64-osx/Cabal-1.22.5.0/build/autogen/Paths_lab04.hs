module Paths_lab04 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/patrickoboyle/Documents/Git/college/CS3016-1617/Lab04/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/bin"
libdir     = "/Users/patrickoboyle/Documents/Git/college/CS3016-1617/Lab04/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/lib/x86_64-osx-ghc-7.10.3/lab04-0.1.0.0-GDC92D3Nb2K93aQUxFkkpE"
datadir    = "/Users/patrickoboyle/Documents/Git/college/CS3016-1617/Lab04/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/share/x86_64-osx-ghc-7.10.3/lab04-0.1.0.0"
libexecdir = "/Users/patrickoboyle/Documents/Git/college/CS3016-1617/Lab04/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/libexec"
sysconfdir = "/Users/patrickoboyle/Documents/Git/college/CS3016-1617/Lab04/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lab04_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lab04_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lab04_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab04_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab04_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
