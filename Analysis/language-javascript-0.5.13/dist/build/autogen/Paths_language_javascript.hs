module Paths_language_javascript (
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
version = Version {versionBranch = [0,5,13], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/alex/.cabal/bin"
libdir     = "/home/alex/.cabal/lib/x86_64-linux-ghc-7.8.3/language-javascript-0.5.13"
datadir    = "/home/alex/.cabal/share/x86_64-linux-ghc-7.8.3/language-javascript-0.5.13"
libexecdir = "/home/alex/.cabal/libexec"
sysconfdir = "/home/alex/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "language_javascript_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "language_javascript_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "language_javascript_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "language_javascript_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "language_javascript_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
