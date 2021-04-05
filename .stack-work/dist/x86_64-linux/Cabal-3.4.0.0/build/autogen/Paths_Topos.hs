{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Topos (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/takaoka/programming/haskell/Topos/.stack-work/install/x86_64-linux/1b6327ce460b46f1b23ef244e2b68fb2b2bd84e50f5aae56955ab7b3dbcbed1d/9.0.1/bin"
libdir     = "/home/takaoka/programming/haskell/Topos/.stack-work/install/x86_64-linux/1b6327ce460b46f1b23ef244e2b68fb2b2bd84e50f5aae56955ab7b3dbcbed1d/9.0.1/lib/x86_64-linux-ghc-9.0.1/Topos-0.1.0.0-7sMPNtmZzH126XZO7URas0"
dynlibdir  = "/home/takaoka/programming/haskell/Topos/.stack-work/install/x86_64-linux/1b6327ce460b46f1b23ef244e2b68fb2b2bd84e50f5aae56955ab7b3dbcbed1d/9.0.1/lib/x86_64-linux-ghc-9.0.1"
datadir    = "/home/takaoka/programming/haskell/Topos/.stack-work/install/x86_64-linux/1b6327ce460b46f1b23ef244e2b68fb2b2bd84e50f5aae56955ab7b3dbcbed1d/9.0.1/share/x86_64-linux-ghc-9.0.1/Topos-0.1.0.0"
libexecdir = "/home/takaoka/programming/haskell/Topos/.stack-work/install/x86_64-linux/1b6327ce460b46f1b23ef244e2b68fb2b2bd84e50f5aae56955ab7b3dbcbed1d/9.0.1/libexec/x86_64-linux-ghc-9.0.1/Topos-0.1.0.0"
sysconfdir = "/home/takaoka/programming/haskell/Topos/.stack-work/install/x86_64-linux/1b6327ce460b46f1b23ef244e2b68fb2b2bd84e50f5aae56955ab7b3dbcbed1d/9.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Topos_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Topos_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Topos_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Topos_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Topos_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Topos_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
