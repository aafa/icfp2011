module Paths_icfpc_ltg (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/aafanasiev/.cabal/bin"
libdir     = "/Users/aafanasiev/.cabal/lib/icfpc-ltg-0.1/ghc-6.12.3"
datadir    = "/Users/aafanasiev/.cabal/share/icfpc-ltg-0.1"
libexecdir = "/Users/aafanasiev/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "icfpc_ltg_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "icfpc_ltg_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "icfpc_ltg_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "icfpc_ltg_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
