module Control.Concurrent.Structured.Base
    ( myThreadId, C.ThreadId
    , fork, fork_
    , forkFinally, forkFinally_
    , killThread
    , CSE.throwTo
    , threadDelay
    ) where

import           Control.Monad (void)
import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO)
import           Control.Concurrent.Structured.Exception as CSE (mask_, try, throwTo)
import qualified Control.Concurrent as C
import qualified Control.Exception as E

myThreadId :: CIO r C.ThreadId
myThreadId = liftIO C.myThreadId
{-# INLINABLE myThreadId #-}

fork :: (a -> IO r') -> CIO r' a -> CIO r C.ThreadId
fork k action = liftIO $ C.forkIO (void $ runCIO (\a -> k a) action)
{-# INLINABLE fork #-}

fork_ :: CIO a a -> CIO r C.ThreadId
fork_ = fork return
{-# INLINABLE fork_ #-}

forkFinally
    :: (a -> IO r')
    -> CIO r' a
    -> (Either E.SomeException r' -> CIO t t)
    -> CIO r C.ThreadId
forkFinally k action finalizer =
    mask_ $ \ restore ->
        fork
            (\ ei -> runCIO return (finalizer ei))
            (try (\ a -> k a) $ restore action)
{-# INLINABLE forkFinally #-}

forkFinally_
    :: CIO a a
    -> (Either E.SomeException a -> CIO r' r')
    -> CIO r C.ThreadId
forkFinally_ = forkFinally (\a -> return a)
{-# INLINABLE forkFinally_ #-}

--forkWithUnmask :: ((forall a. CIO r a -> CIO r a) -> CIO r ()) -> CIO r C.ThreadId
--forkWithUnmask userAction = liftIO $ C.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) ->
--    let
--        unmask :: CIO r a -> CIO r a
--        unmask action = liftIO $ unmaskIO $ runConcurrent return action
--    in
--        runConcurrent return (userAction unmask)

killThread :: C.ThreadId -> CIO r ()
killThread tid = liftIO $ C.killThread tid
{-# INLINABLE killThread #-}

threadDelay :: Int -> CIO r ()
threadDelay n = liftIO $ C.threadDelay n
{-# INLINABLE threadDelay #-}
