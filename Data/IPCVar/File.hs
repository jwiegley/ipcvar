module Data.IPCVar.File where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy.Char8
import Data.IPCVar.Backend
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

encodeFd :: Binary a => Fd -> a -> IO ()
encodeFd fd x = void $ fdWrite fd (unpack (encode x))

decodeFd :: Binary a => Fd -> IO a
decodeFd fd = do
    sz <- fdSeek fd SeekFromEnd 0 -- seek to the end to get the size
    _ <- fdSeek fd AbsoluteSeek 0
    (bs, sz') <- fdRead fd (fromIntegral sz)
    assert (sz == fromIntegral sz') $ return $ decode (pack bs)

fileIPCBackend :: Binary a => FilePath -> IPCVarBackend a
fileIPCBackend path = IPCVarBackend
    { readValue  = withLock path ReadOnly ReadLock decodeFd
    , writeValue = \x -> withLock path WriteOnly WriteLock $ flip encodeFd x
    , swapValue  = \x -> withLock path ReadWrite WriteLock $ \fd ->
        decodeFd fd <* encodeFd fd x
    , deleteValue = removeFile path
    }

newIPCVar :: Binary a => a -> IO (IPCVar a)
newIPCVar x = do
    tmpDir <- getTemporaryDirectory
    (path, h) <- openBinaryTempFile tmpDir "shmvar."
    go path h `onException` removeFile path
  where
    go path h = do
        hClose h
        let var = IPCVar (fileIPCBackend path)
        writeValue (getIPCBackend var) x
        return var
