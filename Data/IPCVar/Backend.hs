{-# LANGUAGE ExistentialQuantification #-}

module Data.IPCVar.Backend where

import Data.Binary

data IPCVarBackend a = Binary a => IPCVarBackend
    { readValue   :: IO a
    , writeValue  :: a -> IO ()
    , swapValue   :: a -> IO a
    , deleteValue :: IO ()
    }

newtype IPCVar a = IPCVar { getIPCBackend :: IPCVarBackend a }
