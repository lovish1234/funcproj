module Paths_Main (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mayank/bin"
libdir     = "/home/mayank/lib/Main-0.0/ghc-7.4.2"
datadir    = "/home/mayank/share/Main-0.0"
libexecdir = "/home/mayank/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Main_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Main_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Main_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Main_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
