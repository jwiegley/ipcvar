{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IPCVar.File (newIPCVar) where

import Control.Applicative
import Control.Exception
import Data.Binary
import Data.IPCVar.Backend
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import System.Directory
import System.IO hiding (hGetContents)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

withLock :: FilePath -> OpenMode -> LockRequest -> (Fd -> IO a) -> IO a
withLock path mode req f = do
    fd <- openFd path mode (Just ownerModes) defaultFileFlags
    go fd `finally` closeFd fd
  where
    go fd =
        (waitToSetLock fd (req, AbsoluteSeek, 0, 0) >> f fd)
            `finally` waitToSetLock fd (Unlock, AbsoluteSeek, 0, 0)

fileIPCBackend :: Binary a => FilePath -> IPCVarBackend a
fileIPCBackend path = IPCVarBackend
    { readValue  = withLock path ReadOnly ReadLock decodeFd
    , writeValue = \x -> withLock path WriteOnly WriteLock $ flip encodeFd x
    , swapValue  = \x -> withLock path ReadWrite WriteLock $ \fd ->
        decodeFd fd <* encodeFd fd x
    , deleteValue = removeFile path
    , encodeState = encodeUtf8 (T.pack path)
    }

-- decodeState :: Binary a => ByteString -> IPCVarBackend a
-- decodeState = fileIPCBackend . T.unpack . decodeUtf8

-- instance Binary a => Binary (IPCVar a) where
--     put (IPCVar b) = put (encodeState b)
--     get = IPCVar . decodeState <$> get

newIPCVar :: Binary a => a -> IO (IPCVar a)
newIPCVar x = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openBinaryTempFile tmpDir "shmvar."
    go path h `onException` removeFile path
  where
    go path h = do
        hClose h
        let var = IPCVar (fileIPCBackend path)
        writeValue (getIPCVarBackend var) x
        return var
