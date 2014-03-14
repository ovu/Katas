module Paths_kata_potter (
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
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/omardiego/Library/Haskell/ghc-7.6.3/lib/kata-potter-0.0.0/bin"
libdir     = "/Users/omardiego/Library/Haskell/ghc-7.6.3/lib/kata-potter-0.0.0/lib"
datadir    = "/Users/omardiego/Library/Haskell/ghc-7.6.3/lib/kata-potter-0.0.0/share"
libexecdir = "/Users/omardiego/Library/Haskell/ghc-7.6.3/lib/kata-potter-0.0.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "kata_potter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kata_potter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kata_potter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kata_potter_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
