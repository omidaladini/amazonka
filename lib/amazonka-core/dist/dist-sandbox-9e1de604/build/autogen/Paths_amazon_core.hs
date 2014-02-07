module Paths_amazon_core (
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
version = Version {versionBranch = [0,1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/brendanhay/Projects/amazonka-types/.cabal-sandbox/bin"
libdir     = "/Users/brendanhay/Projects/amazonka-types/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/amazon-core-0.1.0"
datadir    = "/Users/brendanhay/Projects/amazonka-types/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/amazon-core-0.1.0"
libexecdir = "/Users/brendanhay/Projects/amazonka-types/.cabal-sandbox/libexec"
sysconfdir = "/Users/brendanhay/Projects/amazonka-types/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "amazon_core_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "amazon_core_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "amazon_core_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "amazon_core_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "amazon_core_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
