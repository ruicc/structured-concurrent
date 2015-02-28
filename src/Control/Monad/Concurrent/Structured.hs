module Control.Monad.Concurrent.Structured
    (
    -- * Internal Monad
      CIO, CSTM
    , IO', STM'
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

import           GHC.TypeLits
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.STM as S
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Cont (ContT(..), runContT)

type CIO (n :: Nat) r a = ContT r IO a

type CSTM (n :: Nat) r a = ContT r S.STM a

type Concurrent a = forall (n :: Nat). CIO n () a

type IO' (n :: Nat) a = IO a
type STM' (n :: Nat) a = S.STM a


runCIO :: (a -> IO' n r) -> CIO n r a -> IO r
runCIO k action = runContT action (\a -> k a)
{-# INLINE runCIO #-}

runCSTM :: (a -> STM' n r) -> CSTM n r a -> S.STM r
runCSTM k action = runContT action (\a -> k a)
{-# INLINE runCSTM #-}

atomically :: (a -> STM' n r') -> CSTM n r' a -> CIO n r r'
atomically k action = liftIO $ S.atomically $ runContT action (\a -> k a)
{-# INLINE atomically #-}

atomically_ :: CSTM n r' r' -> CIO n r r'
atomically_ = atomically return
{-# INLINE atomically_ #-}

runConcurrent :: Concurrent a -> IO ()
runConcurrent action = runContT action (const $ return ())
{-# INLINE runConcurrent #-}

runSTM :: S.STM a -> CIO n r a
runSTM action = liftIO $ S.atomically action
{-# INLINE runSTM #-}

liftSTM :: S.STM a -> CSTM n r a
liftSTM action = ContT (\k -> action >>= k)
{-# INLINE liftSTM #-}
