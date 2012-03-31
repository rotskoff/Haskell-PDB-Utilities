module Paths_PDBtools (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/grantrotskoff/Library/Haskell/ghc-7.0.3/lib/PDBtools-0.0.1/bin"
libdir     = "/Users/grantrotskoff/Library/Haskell/ghc-7.0.3/lib/PDBtools-0.0.1/lib"
datadir    = "/Users/grantrotskoff/Library/Haskell/ghc-7.0.3/lib/PDBtools-0.0.1/share"
libexecdir = "/Users/grantrotskoff/Library/Haskell/ghc-7.0.3/lib/PDBtools-0.0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "PDBtools_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "PDBtools_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "PDBtools_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "PDBtools_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
