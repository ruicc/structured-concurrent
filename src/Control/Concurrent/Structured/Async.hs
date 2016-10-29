module Control.Concurrent.Structured.Async
    ( A.Async
    , async, async_, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask
    , withAsync, withAsync_
    , withAsyncBound, withAsyncBound_
    , withAsyncOn, withAsyncOn_
    , wait, poll, waitCatch, cancel, cancelWith, asyncThreadId
    , waitSTM, pollSTM, waitCatchSTM
    , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
    , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel, waitEither_, waitBoth
    , link, link2
    , race, race_, concurrently, concurrently_, mapConcurrently
    , A.Concurrently(..)
    ) where

import           Control.Monad.Concurrent.Structured (CIO, runCIO, CSTM, liftIO, liftSTM)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Control.Exception as E
import           Control.Monad.Trans.Cont (ContT(..), callCC)
import           Data.Traversable (Traversable)


async :: (a -> IO r') -> CIO r' a -> CIO r (A.Async r')
async k m = liftIO $ A.async (runCIO k m)
{-# INLINABLE async #-}

async_ :: CIO r' r' -> CIO r (A.Async r')
async_ = async return
{-# INLINABLE async_ #-}

asyncBound :: (a -> IO r') -> CIO r' a -> CIO r (A.Async r')
asyncBound k m = liftIO $ A.asyncBound (runCIO k m)
{-# INLINABLE asyncBound #-}

asyncOn :: (a -> IO r') -> Int -> CIO r' a -> CIO r (A.Async r')
asyncOn k n m = liftIO $ A.asyncOn n (runCIO k m)
{-# INLINABLE asyncOn #-}

asyncWithUnmask
    :: (a -> IO r')
    -> ((forall s b. CIO s b -> CIO s b) -> CIO r' a)
    -> CIO r (A.Async r')
asyncWithUnmask k action =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- IO
            r' <- A.asyncWithUnmask $ \ unblock ->
                let
                    unmask act = ContT $ \k'' -> unblock (runCIO k'' act)
                in
                    runCIO k (action unmask)
            runCIO k' (exit r')
{-# INLINABLE asyncWithUnmask #-}

asyncOnWithUnmask
    :: (a -> IO r')
    -> Int
    -> ((forall s b. CIO s b -> CIO s b) -> CIO r' a)
    -> CIO r (A.Async r')
asyncOnWithUnmask k n action =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- IO
            r' <- A.asyncOnWithUnmask n $ \ unblock ->
                let
                    unmask act = ContT $ \k'' -> unblock (runCIO k'' act)
                in
                    runCIO k (action unmask)
            runCIO k' (exit r')
{-# INLINABLE asyncOnWithUnmask #-}

withAsync :: (a -> IO a') -> (b -> IO r') -> CIO a' a -> (A.Async a' -> CIO r' b) -> CIO r r'
withAsync k k' action inner = liftIO $ A.withAsync (runCIO k action) (\ asyn -> runCIO k' (inner asyn))
{-# INLINABLE withAsync #-}

withAsync_ :: CIO a a -> (A.Async a -> CIO r' r') -> CIO r r'
withAsync_ = withAsync return return
{-# INLINABLE withAsync_ #-}

withAsyncBound :: (a -> IO a') -> (b -> IO r') -> CIO a' a -> (A.Async a' -> CIO r' b) -> CIO r r'
withAsyncBound k k' action inner = liftIO $ A.withAsyncBound (runCIO k action) (\ asyn -> runCIO k' (inner asyn))
{-# INLINABLE withAsyncBound #-}

withAsyncBound_ :: CIO a a -> (A.Async a -> CIO r' r') -> CIO r r'
withAsyncBound_ = withAsyncBound return return
{-# INLINABLE withAsyncBound_ #-}

withAsyncOn :: (a -> IO a') -> (b -> IO r') -> Int -> CIO a' a -> (A.Async a' -> CIO r' b) -> CIO r r'
withAsyncOn k k' n action inner = liftIO $ A.withAsyncOn n (runCIO k action) (\ asyn -> runCIO k' (inner asyn))
{-# INLINABLE withAsyncOn #-}

withAsyncOn_ :: Int -> CIO a a -> (A.Async a -> CIO r' r') -> CIO r r'
withAsyncOn_ = withAsyncOn return return
{-# INLINABLE withAsyncOn_ #-}



wait :: A.Async a -> CIO r a
wait = liftIO . A.wait
{-# INLINABLE wait #-}

poll :: A.Async a -> CIO r (Maybe (Either E.SomeException a))
poll as = liftIO $ A.poll as
{-# INLINABLE poll #-}

waitCatch :: A.Async a -> CIO r (Either E.SomeException a)
waitCatch as = liftIO $ A.waitCatch as
{-# INLINABLE waitCatch #-}

cancel :: A.Async a -> CIO r ()
cancel as = liftIO $ A.cancel as
{-# INLINABLE cancel #-}

cancelWith :: E.Exception e => A.Async a -> e -> IO ()
cancelWith as e = liftIO $ A.cancelWith as e
{-# INLINABLE cancelWith #-}

asyncThreadId :: A.Async a -> C.ThreadId
asyncThreadId = A.asyncThreadId
{-# INLINABLE asyncThreadId #-}



waitSTM :: A.Async a -> CSTM r a
waitSTM as = liftSTM $ A.waitSTM as
{-# INLINABLE waitSTM #-}

pollSTM :: A.Async a -> CSTM r (Maybe (Either E.SomeException a))
pollSTM as = liftSTM $ A.pollSTM as
{-# INLINABLE pollSTM #-}

waitCatchSTM :: A.Async a -> CSTM r (Either E.SomeException a)
waitCatchSTM as = liftSTM $ A.waitCatchSTM as
{-# INLINABLE waitCatchSTM #-}



waitAny :: [A.Async a] -> CIO r (A.Async a, a)
waitAny ass = liftIO $ A.waitAny ass
{-# INLINABLE waitAny #-}

waitAnyCatch :: [A.Async a] -> CIO r (A.Async a, Either E.SomeException a)
waitAnyCatch ass = liftIO $ A.waitAnyCatch ass
{-# INLINABLE waitAnyCatch #-}

waitAnyCancel :: [A.Async a] -> CIO r (A.Async a, a)
waitAnyCancel ass = liftIO $ A.waitAnyCancel ass
{-# INLINABLE waitAnyCancel #-}

waitAnyCatchCancel :: [A.Async a] -> CIO r (A.Async a, Either E.SomeException a)
waitAnyCatchCancel ass = liftIO $ A.waitAnyCatchCancel ass
{-# INLINABLE waitAnyCatchCancel #-}

waitEither :: A.Async a -> A.Async b -> CIO r (Either a b)
waitEither a b = liftIO $ A.waitEither a b
{-# INLINABLE waitEither #-}

waitEitherCatch :: A.Async a -> A.Async b -> CIO r (Either (Either E.SomeException a) (Either E.SomeException b))
waitEitherCatch a b = liftIO $ A.waitEitherCatch a b
{-# INLINABLE waitEitherCatch #-}

waitEitherCancel :: A.Async a -> A.Async b -> CIO r (Either a b)
waitEitherCancel a b = liftIO $ A.waitEitherCancel a b
{-# INLINABLE waitEitherCancel #-}

waitEitherCatchCancel :: A.Async a -> A.Async b -> CIO r (Either (Either E.SomeException a) (Either E.SomeException b))
waitEitherCatchCancel a b = liftIO $ A.waitEitherCatchCancel a b
{-# INLINABLE waitEitherCatchCancel #-}

waitEither_ :: A.Async a -> A.Async b -> CIO r ()
waitEither_ a b = liftIO $ A.waitEither_ a b
{-# INLINABLE waitEither_ #-}

waitBoth :: A.Async a -> A.Async b -> CIO r (a, b)
waitBoth a b = liftIO $ A.waitBoth a b
{-# INLINABLE waitBoth #-}



link :: A.Async a -> CIO r ()
link a = liftIO $ A.link a
{-# INLINABLE link #-}

link2 :: A.Async a -> A.Async b -> CIO r ()
link2 a b = liftIO $ A.link2 a b
{-# INLINABLE link2 #-}



race :: (a -> IO s) -> (b -> IO t) -> CIO s a -> CIO t b -> CIO r (Either s t)
race k1 k2 c1 c2 = liftIO $ A.race (runCIO k1 c1) (runCIO k2 c2)
{-# INLINABLE race #-}

race_ :: CIO a a -> CIO b b -> CIO r (Either a b)
race_ = race return return
{-# INLINABLE race_ #-}

concurrently :: (a -> IO s) -> (b -> IO t) -> CIO s a -> CIO t b -> CIO r (s, t)
concurrently s t a b = liftIO $ A.concurrently (runCIO s a) (runCIO t b)
{-# INLINABLE concurrently #-}

concurrently_ :: CIO a a -> CIO b b -> CIO r (a, b)
concurrently_ = concurrently return return
{-# INLINABLE concurrently_ #-}

mapConcurrently :: Traversable t => (b -> IO r') -> (a -> CIO r' b) -> t a -> CIO r (t r')
mapConcurrently k action trav = liftIO $ A.mapConcurrently (\a -> runCIO k (action a)) trav
{-# INLINABLE mapConcurrently #-}
