module Control.Concurrent.Structured.STM
    ( retry, orElse, check, catchSTM
    -- * TVar
    , S.TVar
    , newTVar, newTVarCIO, readTVar, readTVarCIO, writeTVar, modifyTVar, modifyTVar'
    , swapTVar, registerDelay, mkWeakTVar
    -- * TChan
    , S.TChan
    , newTChan, newTChanCIO, newBroadcastTChan, dupTChan, cloneTChan, readTChan
    , tryReadTChan, tryPeekTChan, writeTChan, unGetTChan, isEmptyTChan
    -- * TMVar
    , S.TMVar
    , newTMVar, newEmptyTMVar, newTMVarCIO, newEmptyTMVarCIO
    , takeTMVar, putTMVar, readTMVar, tryReadTMVar, swapTMVar
    , tryTakeTMVar, tryPutTMVar, isEmptyTMVar
    -- * TQueue
    , S.TQueue
    , newTQueue, newTQueueCIO, readTQueue, tryReadTQueue, peekTQueue, tryPeekTQueue
    , writeTQueue, unGetTQueue, isEmptyTQueue
    ) where


import           GHC.TypeLits
import           Control.Monad.Concurrent.Structured (CSTM, CIO, liftSTM, liftIO, runCIO, runCSTM, STM')
import           Control.Monad.Trans.Cont (ContT(..), callCC)
import qualified Control.Concurrent.STM as S
import qualified Control.Exception as E
import           System.Mem.Weak (Weak)


retry :: CSTM n r a
retry = liftSTM S.retry
{-# INLINE retry #-}

orElse :: CSTM n r a -> CSTM n r a -> CSTM n r a
orElse m n = ContT $ \ k -> S.orElse (runCSTM (\a -> k a) m) (runCSTM (\a -> k a) n)
{-# INLINE orElse #-}

check :: Bool -> CSTM n r ()
check = liftSTM . S.check
{-# INLINE check #-}

catchSTM
    :: E.Exception e
    => (a -> STM' (n + 1) r')
    -> CSTM (n + 1) r' a
    -> (e -> CSTM (n + 1) r' a)
    -> CSTM n r r'
catchSTM k action handler =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- STM
            r' <- (runCSTM k action) `S.catchSTM` (\ e -> runCSTM k (handler e))
            runCSTM k' (exit r')
{-# INLINE catchSTM #-}

-- | TVar

type TVar = S.TVar
type TMVar = S.TMVar
type TChan = S.TChan
type TQueue = S.TQueue


newTVar :: a -> CSTM n r (TVar a)
newTVar v = liftSTM $ S.newTVar v
{-# INLINE newTVar #-}

newTVarCIO :: a -> CIO n r (TVar a)
newTVarCIO v = liftIO $ S.newTVarIO v
{-# INLINE newTVarCIO #-}

readTVar :: TVar a -> CSTM n r a
readTVar v = liftSTM $ S.readTVar v
{-# INLINE readTVar #-}

readTVarCIO :: TVar a -> CIO n r a
readTVarCIO tv = liftIO $ S.readTVarIO tv
{-# INLINE readTVarCIO #-}

writeTVar :: TVar a -> a -> CSTM n r ()
writeTVar tv v = liftSTM $ S.writeTVar tv v
{-# INLINE writeTVar #-}

modifyTVar :: TVar a -> (a -> a) -> CSTM n r ()
modifyTVar tv v  = liftSTM $ S.modifyTVar tv v
{-# INLINE modifyTVar #-}

modifyTVar' :: TVar a -> (a -> a) -> CSTM n r ()
modifyTVar' tv v = liftSTM $ S.modifyTVar' tv v
{-# INLINE modifyTVar' #-}

swapTVar :: TVar a -> a -> CSTM n r a
swapTVar tv v = liftSTM $ S.swapTVar tv v
{-# INLINE swapTVar #-}

registerDelay :: Int -> CIO n r (TVar Bool)
registerDelay v = liftIO $ S.registerDelay v
{-# INLINE registerDelay #-}

mkWeakTVar :: TVar a -> CIO n () () -> CIO n r (Weak (TVar a))
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO (\a -> return a) finalizer)
{-# INLINE mkWeakTVar #-}

-- | TChan

newTChan :: CSTM n r (TChan a)
newTChan = liftSTM S.newTChan
{-# INLINE newTChan #-}

newTChanCIO :: CIO n r (TChan a)
newTChanCIO = liftIO S.newTChanIO
{-# INLINE newTChanCIO #-}

newBroadcastTChan :: CSTM n r (TChan a)
newBroadcastTChan = liftSTM S.newBroadcastTChan
{-# INLINE newBroadcastTChan #-}

dupTChan :: TChan a -> CSTM n r (TChan a)
dupTChan tc = liftSTM $ S.dupTChan tc
{-# INLINE dupTChan #-}

cloneTChan :: TChan a -> CSTM n r (TChan a)
cloneTChan tc = liftSTM $ S.cloneTChan tc
{-# INLINE cloneTChan #-}

readTChan :: TChan a -> CSTM n r a
readTChan tc = liftSTM $ S.readTChan tc
{-# INLINE readTChan #-}

tryReadTChan :: TChan a -> CSTM n r (Maybe a)
tryReadTChan tc = liftSTM $ S.tryReadTChan tc
{-# INLINE tryReadTChan #-}

tryPeekTChan :: TChan a -> CSTM n r (Maybe a)
tryPeekTChan tc = liftSTM $ S.tryPeekTChan tc
{-# INLINE tryPeekTChan #-}

writeTChan :: TChan a -> a -> CSTM n r ()
writeTChan tc v = liftSTM $ S.writeTChan tc v
{-# INLINE writeTChan #-}

unGetTChan :: TChan a -> a -> CSTM n r ()
unGetTChan tc v = liftSTM $ S.unGetTChan tc v
{-# INLINE unGetTChan #-}

isEmptyTChan :: TChan a -> CSTM n r Bool
isEmptyTChan tc = liftSTM $ S.isEmptyTChan tc
{-# INLINE isEmptyTChan #-}


-- | TMVar

newTMVar :: a -> CSTM n r (TMVar a)
newTMVar v = liftSTM $ S.newTMVar v
{-# INLINE newTMVar #-}

newEmptyTMVar :: CSTM n r (TMVar a)
newEmptyTMVar = liftSTM S.newEmptyTMVar
{-# INLINE newEmptyTMVar #-}

newTMVarCIO :: a -> CIO n r (TMVar a) 
newTMVarCIO v = liftIO $ S.newTMVarIO v
{-# INLINE newTMVarCIO #-}

newEmptyTMVarCIO :: CIO n r (TMVar a) 
newEmptyTMVarCIO = liftIO S.newEmptyTMVarIO
{-# INLINE newEmptyTMVarCIO #-}

takeTMVar :: TMVar a -> CSTM n r a
takeTMVar tm = liftSTM $ S.takeTMVar tm
{-# INLINE takeTMVar #-}

putTMVar :: TMVar a -> a -> CSTM n r ()
putTMVar tm v = liftSTM $ S.putTMVar tm v
{-# INLINE putTMVar #-}

readTMVar :: TMVar a -> CSTM n r a 
readTMVar tm = liftSTM $ S.readTMVar tm
{-# INLINE readTMVar #-}

tryReadTMVar :: TMVar a -> CSTM n r (Maybe a)
tryReadTMVar tm = liftSTM $ S.tryReadTMVar tm
{-# INLINE tryReadTMVar #-}

swapTMVar :: TMVar a -> a -> CSTM n r a
swapTMVar tm v = liftSTM $ S.swapTMVar tm v
{-# INLINE swapTMVar #-}

tryTakeTMVar :: TMVar a -> CSTM n r (Maybe a)
tryTakeTMVar tm = liftSTM $ S.tryTakeTMVar tm
{-# INLINE tryTakeTMVar #-}

tryPutTMVar :: TMVar a -> a -> CSTM n r Bool
tryPutTMVar tm v = liftSTM $ S.tryPutTMVar tm v
{-# INLINE tryPutTMVar #-}

isEmptyTMVar :: TMVar a -> CSTM n r Bool
isEmptyTMVar tm = liftSTM $ S.isEmptyTMVar tm
{-# INLINE isEmptyTMVar #-}

--mkWeakTMVar :: TMVar a -> CIO n () () -> CIO n r (Weak (TMVar a))
--mkWeakTMVar tmv finalizer = liftIO $ S.mkWeakTMVar tmv (runCIO return finalizer)


-- | TQueue

newTQueue :: CSTM n r (TQueue a)
newTQueue = liftSTM S.newTQueue
{-# INLINE newTQueue #-}

newTQueueCIO :: CIO n r (TQueue a)
newTQueueCIO = liftIO S.newTQueueIO
{-# INLINE newTQueueCIO #-}

readTQueue :: TQueue a -> CSTM n r a
readTQueue q = liftSTM $ S.readTQueue q
{-# INLINE readTQueue #-}

tryReadTQueue :: TQueue a -> CSTM n r (Maybe a)
tryReadTQueue q = liftSTM $ S.tryReadTQueue q
{-# INLINE tryReadTQueue #-}

peekTQueue :: TQueue a -> CSTM n r a
peekTQueue q = liftSTM $ S.peekTQueue q
{-# INLINE peekTQueue #-}

tryPeekTQueue :: TQueue a -> CSTM n r (Maybe a)
tryPeekTQueue q = liftSTM $ S.tryPeekTQueue q
{-# INLINE tryPeekTQueue #-}

writeTQueue :: TQueue a -> a -> CSTM n r ()
writeTQueue q v = liftSTM $ S.writeTQueue q v
{-# INLINE writeTQueue #-}

unGetTQueue :: TQueue a -> a -> CSTM n r ()
unGetTQueue q v = liftSTM $ S.unGetTQueue q v
{-# INLINE unGetTQueue #-}

isEmptyTQueue :: TQueue a -> CSTM n r Bool
isEmptyTQueue q = liftSTM $ S.isEmptyTQueue q
{-# INLINE isEmptyTQueue #-}
