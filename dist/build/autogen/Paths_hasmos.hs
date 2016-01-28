module Paths_hasmos (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/xikusa/dev/haskell/Hasmos/.cabal-sandbox/bin"
libdir     = "/home/xikusa/dev/haskell/Hasmos/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/hasmos-0.1.0.0-3mfl2flfXkf7OfyIblpgm3"
datadir    = "/home/xikusa/dev/haskell/Hasmos/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/hasmos-0.1.0.0"
libexecdir = "/home/xikusa/dev/haskell/Hasmos/.cabal-sandbox/libexec"
sysconfdir = "/home/xikusa/dev/haskell/Hasmos/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hasmos_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hasmos_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hasmos_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hasmos_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hasmos_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
