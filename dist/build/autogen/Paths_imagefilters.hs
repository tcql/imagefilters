module Paths_imagefilters (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tchannel/.cabal/bin"
libdir     = "/home/tchannel/.cabal/lib/imagefilters-0.1/ghc-7.0.3"
datadir    = "/home/tchannel/.cabal/share/imagefilters-0.1"
libexecdir = "/home/tchannel/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "imagefilters_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "imagefilters_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "imagefilters_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "imagefilters_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
