module Paths_lucre (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/michal/Library/Haskell/ghc-7.4.1/lib/lucre-0.1/bin"
libdir     = "/Users/michal/Library/Haskell/ghc-7.4.1/lib/lucre-0.1/lib"
datadir    = "/Users/michal/Library/Haskell/ghc-7.4.1/lib/lucre-0.1/share"
libexecdir = "/Users/michal/Library/Haskell/ghc-7.4.1/lib/lucre-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "lucre_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lucre_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lucre_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lucre_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
