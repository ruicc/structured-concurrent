module Main where

import           Test.Tasty
--import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC
import           System.Exit (exitFailure, exitSuccess)

import           Control.Applicative
import           Control.Concurrent.Structured
import           Control.Exception (ErrorCall(..))


main :: IO ()
main = do
    let
        runCIO_ = runCIO return
        tests =
            [ runCIO_ $ do
                tch <- newTChanCIO
                fork_normal tch
            , runCIO_ $ do
                tch <- newTChanCIO
                fork_exception tch
            ]

    b <- all id <$> sequence tests

    if b
        then exitSuccess
        else exitFailure


type TestChan = TChan Int

fork_normal :: TestChan -> CIO r Bool
fork_normal tch = do
    let
        handler _ = atomically_ $ writeTChan tch 2

    notice <- newEmptyMVar

    (`forkFinally_` handler) $ do
        atomically_ $ writeTChan tch 1
        putMVar notice ()

    takeMVar notice

    assertTestChan tch [1, 2]

fork_exception :: TestChan -> CIO r Bool
fork_exception tch = do
    notice <- newEmptyMVar

    let
        handler _ = do
            atomically_ $ writeTChan tch 2
            tryPutMVar notice ()

    (`forkFinally_` handler) $ do
        atomically_ $ writeTChan tch 1
        throwCIO $ ErrorCall "heyhey"

    takeMVar notice

    assertTestChan tch [1, 2]

assertTestChan :: TestChan -> [Int] -> CIO r Bool
assertTestChan tch [] = do
    mx <- atomically_ $ tryReadTChan tch
    case mx of
        Just _ -> return False
        Nothing -> return True
assertTestChan tch (x:xs) = do
    v <- atomically_ $ readTChan tch
    if v == x
        then assertTestChan tch xs
        else return False
