module Control.Concurrent.Structured.STM where


import           Control.Monad.STM (STM)
import           Control.Monad.Concurrent.Structured (CSTM, CIO, liftSTM, liftIO)
import qualified Control.Concurrent.STM as S


retry :: CSTM r a
retry = liftSTM S.retry
{-# INLINE retry #-}

orElse :: CSTM r a -> CSTM r a -> CSTM r a
orElse m n = ContT $ \ k -> S.orElse (runCSTM (\a -> k a) m) (runCSTM (\a -> k a) n)
{-# INLINE orElse #-}

check :: Bool -> CSTM r ()
check = liftSTM . S.check
{-# INLINE check #-}

catchSTM :: Exception e => (a -> STM r') -> CSTM r' a -> (e -> CSTM r' a) -> CSTM r r'
catchSTM k action handler =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- STM
            r' <- (runContT action (\a -> k a)) `S.catchSTM` (\ e -> runContT (handler e) (\a -> k a))
            runContT (exit r') k'
{-# INLINE catchSTM #-}

-- | TVar

newTVar :: a -> CSTM r (TVar a)
newTVar v = liftSTM $ S.newTVar v
{-# INLINE newTVar #-}

newTVarCIO :: a -> CIO r (TVar a)
newTVarCIO v = liftIO $ S.newTVarIO v
{-# INLINE newTVarCIO #-}

readTVar :: TVar a -> CSTM r a
readTVar v = liftSTM $ S.readTVar v
{-# INLINE readTVar #-}

readTVarCIO :: TVar a -> CIO r a
readTVarCIO tv = liftIO $ S.readTVarIO tv
{-# INLINE readTVarCIO #-}

writeTVar :: TVar a -> a -> CSTM r ()
writeTVar tv v = liftSTM $ S.writeTVar tv v
{-# INLINE writeTVar #-}

modifyTVar :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar tv v  = liftSTM $ S.modifyTVar tv v
{-# INLINE modifyTVar #-}

modifyTVar' :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar' tv v = liftSTM $ S.modifyTVar' tv v
{-# INLINE modifyTVar' #-}

swapTVar :: TVar a -> a -> CSTM r a
swapTVar tv v = liftSTM $ S.swapTVar tv v
{-# INLINE swapTVar #-}

registerDelay :: Int -> CIO r (TVar Bool)
registerDelay v = liftIO $ S.registerDelay v
{-# INLINE registerDelay #-}

mkWeakTVar :: TVar a -> CIO () () -> CIO r (Weak (TVar a))
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO (\a -> return a) finalizer)
{-# INLINE mkWeakTVar #-}

-- | TChan

newTChan :: CSTM r (TChan a)
newTChan = liftSTM S.newTChan
{-# INLINE newTChan #-}

newTChanCIO :: CIO r (TChan a)
newTChanCIO = liftIO S.newTChanIO
{-# INLINE newTChanCIO #-}

newBroadcastTChan :: CSTM r (TChan a)
newBroadcastTChan = liftSTM S.newBroadcastTChan
{-# INLINE newBroadcastTChan #-}

dupTChan :: TChan a -> CSTM r (TChan a)
dupTChan tc = liftSTM $ S.dupTChan tc
{-# INLINE dupTChan #-}

cloneTChan :: TChan a -> CSTM r (TChan a)
cloneTChan tc = liftSTM $ S.cloneTChan tc
{-# INLINE cloneTChan #-}

readTChan :: TChan a -> CSTM r a
readTChan tc = liftSTM $ S.readTChan tc
{-# INLINE readTChan #-}

tryReadTChan :: TChan a -> CSTM r (Maybe a)
tryReadTChan tc = liftSTM $ S.tryReadTChan tc
{-# INLINE tryReadTChan #-}

tryPeekTChan :: TChan a -> CSTM r (Maybe a)
tryPeekTChan tc = liftSTM $ S.tryPeekTChan tc
{-# INLINE tryPeekTChan #-}

writeTChan :: TChan a -> a -> CSTM r ()
writeTChan tc v = liftSTM $ S.writeTChan tc v
{-# INLINE writeTChan #-}

unGetTChan :: TChan a -> a -> CSTM r ()
unGetTChan tc v = liftSTM $ S.unGetTChan tc v
{-# INLINE unGetTChan #-}

isEmptyTChan :: TChan a -> CSTM r Bool
isEmptyTChan tc = liftSTM $ S.isEmptyTChan tc
{-# INLINE isEmptyTChan #-}


-- | TMVar

newTMVar :: a -> CSTM r (TMVar a)
newTMVar v = liftSTM $ S.newTMVar v
{-# INLINE newTMVar #-}

newEmptyTMVar :: CSTM r (TMVar a)
newEmptyTMVar = liftSTM S.newEmptyTMVar
{-# INLINE newEmptyTMVar #-}

newTMVarCIO :: a -> CIO r (TMVar a) 
newTMVarCIO v = liftIO $ S.newTMVarIO v
{-# INLINE newTMVarCIO #-}

newEmptyTMVarCIO :: CIO r (TMVar a) 
newEmptyTMVarCIO = liftIO S.newEmptyTMVarIO
{-# INLINE newEmptyTMVarCIO #-}

takeTMVar :: TMVar a -> CSTM r a
takeTMVar tm = liftSTM $ S.takeTMVar tm
{-# INLINE takeTMVar #-}

putTMVar :: TMVar a -> a -> CSTM r ()
putTMVar tm v = liftSTM $ S.putTMVar tm v
{-# INLINE putTMVar #-}

readTMVar :: TMVar a -> CSTM r a 
readTMVar tm = liftSTM $ S.readTMVar tm
{-# INLINE readTMVar #-}

tryReadTMVar :: TMVar a -> CSTM r (Maybe a)
tryReadTMVar tm = liftSTM $ S.tryReadTMVar tm
{-# INLINE tryReadTMVar #-}

swapTMVar :: TMVar a -> a -> CSTM r a
swapTMVar tm v = liftSTM $ S.swapTMVar tm v
{-# INLINE swapTMVar #-}

tryTakeTMVar :: TMVar a -> CSTM r (Maybe a)
tryTakeTMVar tm = liftSTM $ S.tryTakeTMVar tm
{-# INLINE tryTakeTMVar #-}

tryPutTMVar :: TMVar a -> a -> CSTM r Bool
tryPutTMVar tm v = liftSTM $ S.tryPutTMVar tm v
{-# INLINE tryPutTMVar #-}

isEmptyTMVar :: TMVar a -> CSTM r Bool
isEmptyTMVar tm = liftSTM $ S.isEmptyTMVar tm
{-# INLINE isEmptyTMVar #-}

--mkWeakTMVar :: TMVar a -> CIO () () -> CIO r (Weak (TMVar a))
--mkWeakTMVar tmv finalizer = liftIO $ S.mkWeakTMVar tmv (runCIO return finalizer)


-- | TQueue

type TQueue = S.TQueue

newTQueue :: CSTM r (TQueue a)
newTQueue = liftSTM S.newTQueue
{-# INLINE newTQueue #-}

newTQueueCIO :: CIO r (TQueue a)
newTQueueCIO = liftIO S.newTQueueIO
{-# INLINE newTQueueCIO #-}

readTQueue :: TQueue a -> CSTM r a
readTQueue q = liftSTM $ S.readTQueue q
{-# INLINE readTQueue #-}

tryReadTQueue :: TQueue a -> CSTM r (Maybe a)
tryReadTQueue q = liftSTM $ S.tryReadTQueue q
{-# INLINE tryReadTQueue #-}

peekTQueue :: TQueue a -> CSTM r a
peekTQueue q = liftSTM $ S.peekTQueue q
{-# INLINE peekTQueue #-}

tryPeekTQueue :: TQueue a -> CSTM r (Maybe a)
tryPeekTQueue q = liftSTM $ S.tryPeekTQueue q
{-# INLINE tryPeekTQueue #-}

writeTQueue :: TQueue a -> a -> CSTM r ()
writeTQueue q v = liftSTM $ S.writeTQueue q v
{-# INLINE writeTQueue #-}

unGetTQueue :: TQueue a -> a -> CSTM r ()
unGetTQueue q v = liftSTM $ S.unGetTQueue q v
{-# INLINE unGetTQueue #-}

isEmptyTQueue :: TQueue a -> CSTM r Bool
isEmptyTQueue q = liftSTM $ S.isEmptyTQueue q
{-# INLINE isEmptyTQueue #-}
