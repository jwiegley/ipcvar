module Main where

import Control.Concurrent
import Data.IPCVar
import Data.IPCVar.File as File
import Data.IPCVar.Shm as Shm
import System.Posix.Process
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "file backend" $
        it "reads and writes values" $ do
            var <- File.newIPCVar (10 :: Int)
            _ <- forkProcess $ writeIPCVar var 20
            threadDelay 2000000
            x <- readIPCVar var
            x `shouldBe` 20
            deleteIPCVar var

    describe "shm backend" $
        it "reads and writes values" $ do
            var <- Shm.newIPCVar (10 :: Int)
            _ <- forkProcess $ writeIPCVar var 20
            threadDelay 2000000
            x <- readIPCVar var
            x `shouldBe` 20
            deleteIPCVar var
