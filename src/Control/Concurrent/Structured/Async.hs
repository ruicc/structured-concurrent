module Control.Concurrent.Structured.Async
    ( async
    , wait
    , race, race_
    ) where

import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO)
import qualified Control.Concurrent.Async as A


async :: CIO a a -> CIO r (A.Async a)
async m = liftIO $ A.async (runCIO return m)
{-# INLINE async #-}

wait :: A.Async a -> CIO r a
wait = liftIO . A.wait
{-# INLINE wait #-}

race :: CIO a a -> CIO b b -> CIO r (Either a b)
race c1 c2 = liftIO $ A.race (runCIO return c1) (runCIO return c2)
{-# INLINE race #-}

race_ :: CIO a a -> CIO b b -> CIO r ()
race_ c1 c2 = liftIO $ A.race_ (runCIO return c1) (runCIO return c2)
{-# INLINE race_ #-}
