module Control.Monad.Concurrent.Structured where


type CIO r a = ContT r IO a

type CSTM r a = ContT r S.STM a

type Concurrent a = CIO () a


runCIO :: (a -> IO r) -> CIO r a -> IO r
runCIO k action = runContT action (\a -> k a)
{-# INLINE runCIO #-}

runCSTM :: (a -> S.STM r) -> CSTM r a -> S.STM r
runCSTM k action = runContT action (\a -> k a)
{-# INLINE runCSTM #-}

atomically :: (a -> S.STM r') -> CSTM r' a -> CIO r r'
atomically k action = liftIO $ S.atomically $ runContT action (\a -> k a)
{-# INLINE atomically #-}

atomically_ :: CSTM r' r' -> CIO r r'
atomically_ = atomically return
{-# INLINE atomically_ #-}

runConcurrent :: Concurrent a -> IO ()
runConcurrent action = runContT action (const $ return ())
{-# INLINE runConcurrent #-}

runSTM :: S.STM a -> CIO r a
runSTM m = liftIO $ S.atomically m
{-# INLINE runSTM #-}

liftSTM :: S.STM a -> CSTM r a
liftSTM m = ContT (\k -> m >>= k)
{-# INLINE liftSTM #-}
