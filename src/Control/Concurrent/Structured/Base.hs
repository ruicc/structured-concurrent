module Control.Concurrent.Structured.Base
    ( myThreadId, C.ThreadId
    , fork, fork_
    , forkFinally, forkFinally_
    , killThread
    , threadDelay
    ) where

import           GHC.TypeLits
import           Control.Monad (void)
import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO, IO')
import           Control.Concurrent.Structured.Exception (mask_, try)
import qualified Control.Concurrent as C
import qualified Control.Exception as E

myThreadId :: CIO n r C.ThreadId
myThreadId = liftIO C.myThreadId
{-# INLINE myThreadId #-}

fork :: (a -> IO' (n + 1) r') -> CIO (n + 1) r' a -> CIO n r C.ThreadId
fork k action = liftIO $ C.forkIO (void $ runCIO (\a -> k a) action)
{-# INLINE fork #-}

fork_ :: CIO (n + 1) a a -> CIO n r C.ThreadId
fork_ = fork return
{-# INLINE fork_ #-}

forkFinally
    :: (a -> IO' (n + 1) r')
    -> CIO (n + 1) r' a
    -> (Either E.SomeException r' -> CIO (n + 1) r'' r'')
    -> CIO n r C.ThreadId
forkFinally k action finalizer =
    mask_ $ \ restore ->
        fork
            (\ ei -> runCIO return (finalizer ei))
            (try (\ a -> k a) $ restore action)
{-# INLINE forkFinally #-}

forkFinally_
    :: CIO (n + 1) a a
    -> (Either E.SomeException a -> CIO (n + 1) r' r')
    -> CIO n r C.ThreadId
forkFinally_ = forkFinally (\a -> return a)
{-# INLINE forkFinally_ #-}

--forkWithUnmask :: ((forall a. CIO r a -> CIO r a) -> CIO r ()) -> CIO r C.ThreadId
--forkWithUnmask userAction = liftIO $ C.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) ->
--    let
--        unmask :: CIO r a -> CIO r a
--        unmask action = liftIO $ unmaskIO $ runConcurrent return action
--    in
--        runConcurrent return (userAction unmask)

killThread :: C.ThreadId -> CIO n r ()
killThread tid = liftIO $ C.killThread tid
{-# INLINE killThread #-}

threadDelay :: Int -> CIO n r ()
threadDelay n = liftIO $ C.threadDelay n
{-# INLINE threadDelay #-}
