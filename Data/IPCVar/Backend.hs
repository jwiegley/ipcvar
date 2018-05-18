{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.IPCVar.Backend where

import Control.Exception
import Data.Binary
import Data.ByteString.Lazy
import Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding as Text
import System.IO hiding (hGetContents)
import System.Posix.IO
import System.Posix.Types

data IPCVarBackend a = Binary a => IPCVarBackend
    { readValue   :: IO a
    , writeValue  :: a -> IO ()
    , swapValue   :: a -> IO a
    , deleteValue :: IO ()
    , encodeState :: ByteString
    }

newtype IPCVar a = IPCVar { getIPCVarBackend :: IPCVarBackend a }

-- newtype IPCMVar a = IPCMVar { getIPCMVarBackend :: IPCVarBackend a }

encodeFd :: Binary a => Fd -> a -> IO ()
encodeFd fd x = do
    !_ <- fdWrite fd (Text.unpack (Text.decodeUtf8 (encode x)))
    return ()

decodeFd :: Binary a => Fd -> IO a
decodeFd fd = do
    sz <- fdSeek fd SeekFromEnd 0 -- seek to the end to get the size
    !_ <- fdSeek fd AbsoluteSeek 0
    (!bs, !sz') <- fdRead fd (fromIntegral sz)
    assert (sz == fromIntegral sz') $
        return $ decode (Text.encodeUtf8 (Text.pack bs))
