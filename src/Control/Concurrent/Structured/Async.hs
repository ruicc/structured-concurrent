module Control.Concurrent.Structured.Async where

import           Control.Monad.Concurrent.Structured


async :: CIO a a -> CIO r (As.Async a)
async m = liftIO $ As.async (runCIO return m)
{-# INLINE async #-}

wait :: Async a -> CIO r a 
wait = liftIO . As.wait
{-# INLINE wait #-}

race :: CIO a a -> CIO b b -> CIO r (Either a b)
race c1 c2 = liftIO $ As.race (runCIO return c1) (runCIO return c2)
{-# INLINE race #-}

race_ :: CIO a a -> CIO b b -> CIO r ()
race_ c1 c2 = liftIO $ As.race_ (runCIO return c1) (runCIO return c2)
{-# INLINE race_ #-}
