module Paths_project_euler (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mitchell/Library/Haskell/bin"
libdir     = "/Users/mitchell/Library/Haskell/ghc-7.8.3-x86_64/lib/project-euler-0.1.0.0"
datadir    = "/Users/mitchell/Library/Haskell/share/ghc-7.8.3-x86_64/project-euler-0.1.0.0"
libexecdir = "/Users/mitchell/Library/Haskell/libexec"
sysconfdir = "/Users/mitchell/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project_euler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project_euler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "project_euler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project_euler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project_euler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
