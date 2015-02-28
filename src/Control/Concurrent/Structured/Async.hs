module Control.Concurrent.Structured.Async
    ( A.Async
    , async
    , wait
    , race, race_
    ) where

import           GHC.TypeLits
import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO)
import qualified Control.Concurrent.Async as A


async :: CIO (n + 1) a a -> CIO n r (A.Async a)
async m = liftIO $ A.async (runCIO return m)
{-# INLINE async #-}

wait :: A.Async a -> CIO n r a
wait = liftIO . A.wait
{-# INLINE wait #-}

race :: CIO (n + 1) a a -> CIO (n + 1) b b -> CIO n r (Either a b)
race c1 c2 = liftIO $ A.race (runCIO return c1) (runCIO return c2)
{-# INLINE race #-}

race_ :: CIO (n + 1) a a -> CIO (n + 1) b b -> CIO n r ()
race_ c1 c2 = liftIO $ A.race_ (runCIO return c1) (runCIO return c2)
{-# INLINE race_ #-}
