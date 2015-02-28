module Main where

import           GHC.TypeLits
import           Test.Tasty
--import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC
import           System.Exit (exitFailure, exitSuccess)

import           Control.Applicative
import           Control.Monad.Trans.Cont
import           Control.Concurrent.Structured
import           Control.Exception (ErrorCall(..))

--runRoot :: (a -> IO' 0 r) -> CIO 0 r a -> IO r
--runRoot k action = runContT action (\a -> k a)
--
main :: IO ()
main = do
    b <- all id <$> (sequence $ map (runCIO return) tests)

    if b
        then exitSuccess
        else exitFailure

tests :: [CIO n r Bool]
tests =
    [ test_fork_normal
    , test_fork_exception
    , test_catch_normal
    , test_catch_exception
    , test_illegal_loop
    ]

type TestChan = TChan Int

test_fork_normal :: CIO n r Bool
test_fork_normal = do
    tch <- newTChanCIO
    let
        write n = atomically_ $ writeTChan tch n
        handler _ = write 2

    notice <- newEmptyMVar

    (`forkFinally_` handler) $ do
        write 1
        putMVar notice ()

    takeMVar notice

    assertTestChan tch [1, 2]

test_fork_exception :: CIO n r Bool
test_fork_exception = do
    tch <- newTChanCIO
    notice <- newEmptyMVar

    let
        write n = atomically_ $ writeTChan tch n
        handler _ = do
            write 2
            tryPutMVar notice ()

    _ <- (`forkFinally_` handler) $ do
        write 1
        throwErr

    takeMVar notice

    assertTestChan tch [1, 2]

test_catch_normal :: CIO n r Bool
test_catch_normal = do
    tch <- newTChanCIO

    let
        write n = atomically_ $ writeTChan tch n
        action = write 1
        handler (_ :: SomeException) = write 2

    action `catch_` handler

    assertTestChan tch [1]

test_catch_exception :: CIO n r Bool
test_catch_exception = do
    tch <- newTChanCIO

    let
        write n = atomically_ $ writeTChan tch n
        action = do
            write 1
            throwErr
        handler (_ :: SomeException) = write 2

    action `catch_` handler

    assertTestChan tch [1, 2]

---- This should be compile error..
--test_illegal_loop :: CIO n r Bool
--test_illegal_loop = do
--    tch <- newTChanCIO
--
--    let
--        write n = atomically_ $ writeTChan tch n
--        action = do
--            write 1
--            throwErr
--        handler (_ :: SomeException) = write 2
--
--        loop :: CIO n r ()
--        loop = handle_ handler $ do
--            loop
--
--    loop
--
--    assertTestChan tch [1, 2]

--------------------------------------------------------------------------------
-- Helper

throwErr = throwCIO $ ErrorCall "heyhey"

assertTestChan :: TestChan -> [Int] -> CIO n r Bool
assertTestChan tch [] = do
    mx <- atomically_ $ tryReadTChan tch
    case mx of
        Just _ -> return False
        Nothing -> return True
assertTestChan tch (x:xs) = do
    mv <- atomically_ $ tryReadTChan tch
    case mv of
        Just v | v == x -> assertTestChan tch xs
        _ -> return False
