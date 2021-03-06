module Control.Monad.Concurrent.Structured
    (
    -- * Internal Monad
      CIO, CSTM
    , runCIO, runCSTM
    , atomically, atomically_
    -- * External Monad
    , Concurrent
    , runConcurrent
    -- ** Helper functions
    , runSTM
    , liftSTM
    , liftIO
    ) where

import qualified Control.Concurrent.STM as S
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Cont (ContT(..), runContT)

type CIO r a = ContT r IO a

type CSTM r a = ContT r S.STM a

type Concurrent a = CIO () a


runCIO :: (a -> IO r) -> CIO r a -> IO r
runCIO k action = runContT action (\a -> k a)
{-# INLINABLE runCIO #-}

runCSTM :: (a -> S.STM r) -> CSTM r a -> S.STM r
runCSTM k action = runContT action (\a -> k a)
{-# INLINABLE runCSTM #-}

atomically :: (a -> S.STM r') -> CSTM r' a -> CIO r r'
atomically k action = liftIO $ S.atomically $ runContT action (\a -> k a)
{-# INLINABLE atomically #-}

atomically_ :: CSTM r' r' -> CIO r r'
atomically_ = atomically return
{-# INLINABLE atomically_ #-}

runConcurrent :: Concurrent a -> IO ()
runConcurrent action = runContT action (const $ return ())
{-# INLINABLE runConcurrent #-}

runSTM :: S.STM a -> CIO r a
runSTM action = liftIO $ S.atomically action
{-# INLINABLE runSTM #-}

liftSTM :: S.STM a -> CSTM r a
liftSTM action = ContT (\k -> action >>= k)
{-# INLINABLE liftSTM #-}
