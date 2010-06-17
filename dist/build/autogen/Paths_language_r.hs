module Paths_language_r (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/language-r-0.1.6/ghc-6.12.2"
datadir    = "/usr/doc/share/language-r-0.1.6"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "language_r_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "language_r_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "language_r_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "language_r_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
