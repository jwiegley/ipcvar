module Data.IPCVar where

import Data.Binary
import Data.IPCVar.Backend

writeIPCVar :: Binary a => IPCVar a -> a -> IO ()
writeIPCVar (IPCVar b) x = writeValue b x

readIPCVar :: Binary a => IPCVar a -> IO a
readIPCVar (IPCVar b) = readValue b

swapIPCVar :: Binary a => IPCVar a -> a -> IO a
swapIPCVar (IPCVar b) x = swapValue b x

deleteIPCVar :: IPCVar a -> IO ()
deleteIPCVar (IPCVar b) = deleteValue b
