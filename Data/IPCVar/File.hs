{-# LANGUAGE OverloadedStrings #-}

module Data.IPCVar.File where

import Control.Applicative
import Control.Exception
import Data.Binary
import Data.IPCVar.Backend
import System.Directory
import System.IO hiding (hGetContents)
import System.Posix.IO

withLock :: FilePath -> IOMode -> LockRequest -> IO a -> IO a
withLock path mode req f = withBinaryFile path mode $ \h -> do
    fd <- handleToFd h
    (waitToSetLock fd (req, AbsoluteSeek, 0, 0) >> f)
        `finally` waitToSetLock fd (Unlock, AbsoluteSeek, 0, 0)

fileIPCBackend :: Binary a => FilePath -> IPCVarBackend a
fileIPCBackend path = IPCVarBackend
    { readValue  = withLock path ReadMode ReadLock (decodeFile path)
    , writeValue = withLock path WriteMode WriteLock . encodeFile path
    , swapValue  = \x -> withLock path ReadWriteMode WriteLock $
        decodeFile path <* encodeFile path x
    , deleteValue = removeFile path
    }

newIPCVar :: Binary a => a -> IO (IPCVar a)
newIPCVar x = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openTempFile tmpDir "shmvar."
    go path h `onException` removeFile path
  where
    go path h = do
        hClose h
        let var = IPCVar (fileIPCBackend path)
        writeValue (getIPCBackend var) x
        return var
