module Control.Concurrent.Structured.Base
    ( myThreadId, C.ThreadId
    , fork, fork_
    , forkFinally, forkFinally_
    , killThread
    , threadDelay
    ) where

import           Control.Monad (void)
import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO)
import           Control.Concurrent.Structured.Exception (mask_, try)
import qualified Control.Concurrent as C
import qualified Control.Exception as E

myThreadId :: CIO r C.ThreadId
myThreadId = liftIO C.myThreadId
{-# INLINE myThreadId #-}

fork :: (a -> IO r') -> CIO r' a -> CIO r C.ThreadId
fork k action = liftIO $ C.forkIO (void $ runCIO (\a -> k a) action)
{-# INLINE fork #-}

fork_ :: CIO a a -> CIO r C.ThreadId
fork_ = fork return
{-# INLINE fork_ #-}

forkFinally
    :: (a -> IO r')
    -> CIO r' a
    -> (Either E.SomeException r' -> CIO r'' r'')
    -> CIO r C.ThreadId
forkFinally k action finalizer =
    mask_ $ \ restore ->
        fork
            (\ ei -> runCIO return (finalizer ei))
            (try (\ a -> k a) $ restore action)
{-# INLINE forkFinally #-}

forkFinally_
    :: CIO a a
    -> (Either E.SomeException a -> CIO r' r')
    -> CIO r C.ThreadId
forkFinally_ = forkFinally (\a -> return a)
{-# INLINE forkFinally_ #-}

--forkWithUnmask :: ((forall a. CIO r a -> CIO r a) -> CIO r ()) -> CIO r C.ThreadId
--forkWithUnmask userAction = liftIO $ C.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) ->
--    let
--        unmask :: CIO r a -> CIO r a
--        unmask action = liftIO $ unmaskIO $ runConcurrent return action
--    in
--        runConcurrent return (userAction unmask)

killThread :: C.ThreadId -> CIO r ()
killThread tid = liftIO $ C.killThread tid
{-# INLINE killThread #-}

threadDelay :: Int -> CIO r ()
threadDelay n = liftIO $ C.threadDelay n
{-# INLINE threadDelay #-}
