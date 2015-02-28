module Control.Concurrent.Structured.Async
    ( A.Async
    , async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask
    , wait
    , race, race_
    ) where

import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO)
import qualified Control.Concurrent.Async as A
import           Control.Monad.Trans.Cont (ContT(..), callCC)


async :: (a -> IO r') -> CIO r' a -> CIO r (A.Async r')
async k m = liftIO $ A.async (runCIO k m)
{-# INLINE async #-}

asyncBound :: (a -> IO r') -> CIO r' a -> CIO r (A.Async r')
asyncBound k m = liftIO $ A.asyncBound (runCIO k m)
{-# INLINE asyncBound #-}

asyncOn :: (a -> IO r') -> Int -> CIO r' a -> CIO r (A.Async r')
asyncOn k n m = liftIO $ A.asyncOn n (runCIO k m)
{-# INLINE asyncOn #-}

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
{-# INLINE asyncWithUnmask #-}

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
{-# INLINE asyncOnWithUnmask #-}

wait :: A.Async a -> CIO r a
wait = liftIO . A.wait
{-# INLINE wait #-}

race :: CIO a a -> CIO b b -> CIO r (Either a b)
race c1 c2 = liftIO $ A.race (runCIO return c1) (runCIO return c2)
{-# INLINE race #-}

race_ :: CIO a a -> CIO b b -> CIO r ()
race_ c1 c2 = liftIO $ A.race_ (runCIO return c1) (runCIO return c2)
{-# INLINE race_ #-}
