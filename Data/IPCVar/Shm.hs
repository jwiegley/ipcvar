{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IPCVar.Shm where

import Control.Applicative
import Control.Exception
import Data.Binary
import Data.ByteString.Lazy.Char8 as BS
import Data.IPCVar.Backend
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.UUID.V4
import System.Posix.Files
import System.Posix.IO
import System.Posix.SharedMem

shmIPCBackend :: Binary a => String -> IPCVarBackend a
shmIPCBackend name = IPCVarBackend
    { readValue  = bracket
        (shmOpen name (ShmOpenFlags False False False False) ownerModes)
        closeFd decodeFd
    , writeValue = bracket
        (shmOpen name (ShmOpenFlags True True False False) ownerModes)
        closeFd . flip encodeFd
    , swapValue  = \x -> bracket
        (shmOpen name (ShmOpenFlags True False False False) ownerModes)
        closeFd (\fd -> decodeFd fd <* encodeFd fd x)
    , deleteValue = shmUnlink name
    , encodeState = encodeUtf8 (T.pack name)
    }

decodeState :: Binary a => ByteString -> IPCVarBackend a
decodeState = shmIPCBackend . T.unpack . decodeUtf8

-- instance Binary a => Binary (IPCVar a) where
--     put (IPCVar b) = put (encodeState b)
--     get = IPCVar . decodeState <$> get

newIPCVar :: Binary a => a -> IO (IPCVar a)
newIPCVar x = do
    uuid <- nextRandom
    -- Mac OS X limits this to PSHMNAMLEN, which is 31 on Mountain Lion.
    let name = Prelude.take 30 (show uuid)
        var = IPCVar (shmIPCBackend name)
    writeValue (getIPCVarBackend var) x
    return var
