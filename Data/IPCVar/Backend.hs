{-# LANGUAGE ExistentialQuantification #-}

module Data.IPCVar.Backend where

import Data.Binary
import Data.ByteString.Lazy

data IPCVarBackend a = Binary a => IPCVarBackend
    { readValue   :: IO a
    , writeValue  :: a -> IO ()
    , swapValue   :: a -> IO a
    , deleteValue :: IO ()
    , encodeState :: ByteString
    }

newtype IPCVar a = IPCVar { getIPCVarBackend :: IPCVarBackend a }

-- newtype IPCMVar a = IPCMVar { getIPCMVarBackend :: IPCVarBackend a }
