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


import           Control.Monad.Concurrent.Structured (CSTM, CIO, liftSTM, liftIO, runCIO, runCSTM)
import           Control.Monad.STM (STM)
import           Control.Monad.Trans.Cont (ContT(..), callCC)
import qualified Control.Concurrent.STM as S
import qualified Control.Exception as E
import           System.Mem.Weak (Weak)


retry :: CSTM r a
retry = liftSTM S.retry
{-# INLINABLE retry #-}

orElse :: CSTM r a -> CSTM r a -> CSTM r a
orElse m n = ContT $ \ k -> S.orElse (runCSTM (\a -> k a) m) (runCSTM (\a -> k a) n)
{-# INLINABLE orElse #-}

check :: Bool -> CSTM r ()
check = liftSTM . S.check
{-# INLINABLE check #-}

catchSTM :: E.Exception e => (a -> STM r') -> CSTM r' a -> (e -> CSTM r' a) -> CSTM r r'
catchSTM k action handler =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- STM
            r' <- (runCSTM k action) `S.catchSTM` (\ e -> runCSTM k (handler e))
            runCSTM k' (exit r')
{-# INLINABLE catchSTM #-}

-- | TVar

type TVar = S.TVar
type TMVar = S.TMVar
type TChan = S.TChan
type TQueue = S.TQueue


newTVar :: a -> CSTM r (TVar a)
newTVar v = liftSTM $ S.newTVar v
{-# INLINABLE newTVar #-}

newTVarCIO :: a -> CIO r (TVar a)
newTVarCIO v = liftIO $ S.newTVarIO v
{-# INLINABLE newTVarCIO #-}

readTVar :: TVar a -> CSTM r a
readTVar v = liftSTM $ S.readTVar v
{-# INLINABLE readTVar #-}

readTVarCIO :: TVar a -> CIO r a
readTVarCIO tv = liftIO $ S.readTVarIO tv
{-# INLINABLE readTVarCIO #-}

writeTVar :: TVar a -> a -> CSTM r ()
writeTVar tv v = liftSTM $ S.writeTVar tv v
{-# INLINABLE writeTVar #-}

modifyTVar :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar tv v  = liftSTM $ S.modifyTVar tv v
{-# INLINABLE modifyTVar #-}

modifyTVar' :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar' tv v = liftSTM $ S.modifyTVar' tv v
{-# INLINABLE modifyTVar' #-}

swapTVar :: TVar a -> a -> CSTM r a
swapTVar tv v = liftSTM $ S.swapTVar tv v
{-# INLINABLE swapTVar #-}

registerDelay :: Int -> CIO r (TVar Bool)
registerDelay v = liftIO $ S.registerDelay v
{-# INLINABLE registerDelay #-}

mkWeakTVar :: TVar a -> CIO () () -> CIO r (Weak (TVar a))
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO (\a -> return a) finalizer)
{-# INLINABLE mkWeakTVar #-}

-- | TChan

newTChan :: CSTM r (TChan a)
newTChan = liftSTM S.newTChan
{-# INLINABLE newTChan #-}

newTChanCIO :: CIO r (TChan a)
newTChanCIO = liftIO S.newTChanIO
{-# INLINABLE newTChanCIO #-}

newBroadcastTChan :: CSTM r (TChan a)
newBroadcastTChan = liftSTM S.newBroadcastTChan
{-# INLINABLE newBroadcastTChan #-}

dupTChan :: TChan a -> CSTM r (TChan a)
dupTChan tc = liftSTM $ S.dupTChan tc
{-# INLINABLE dupTChan #-}

cloneTChan :: TChan a -> CSTM r (TChan a)
cloneTChan tc = liftSTM $ S.cloneTChan tc
{-# INLINABLE cloneTChan #-}

readTChan :: TChan a -> CSTM r a
readTChan tc = liftSTM $ S.readTChan tc
{-# INLINABLE readTChan #-}

tryReadTChan :: TChan a -> CSTM r (Maybe a)
tryReadTChan tc = liftSTM $ S.tryReadTChan tc
{-# INLINABLE tryReadTChan #-}

tryPeekTChan :: TChan a -> CSTM r (Maybe a)
tryPeekTChan tc = liftSTM $ S.tryPeekTChan tc
{-# INLINABLE tryPeekTChan #-}

writeTChan :: TChan a -> a -> CSTM r ()
writeTChan tc v = liftSTM $ S.writeTChan tc v
{-# INLINABLE writeTChan #-}

unGetTChan :: TChan a -> a -> CSTM r ()
unGetTChan tc v = liftSTM $ S.unGetTChan tc v
{-# INLINABLE unGetTChan #-}

isEmptyTChan :: TChan a -> CSTM r Bool
isEmptyTChan tc = liftSTM $ S.isEmptyTChan tc
{-# INLINABLE isEmptyTChan #-}


-- | TMVar

newTMVar :: a -> CSTM r (TMVar a)
newTMVar v = liftSTM $ S.newTMVar v
{-# INLINABLE newTMVar #-}

newEmptyTMVar :: CSTM r (TMVar a)
newEmptyTMVar = liftSTM S.newEmptyTMVar
{-# INLINABLE newEmptyTMVar #-}

newTMVarCIO :: a -> CIO r (TMVar a) 
newTMVarCIO v = liftIO $ S.newTMVarIO v
{-# INLINABLE newTMVarCIO #-}

newEmptyTMVarCIO :: CIO r (TMVar a) 
newEmptyTMVarCIO = liftIO S.newEmptyTMVarIO
{-# INLINABLE newEmptyTMVarCIO #-}

takeTMVar :: TMVar a -> CSTM r a
takeTMVar tm = liftSTM $ S.takeTMVar tm
{-# INLINABLE takeTMVar #-}

putTMVar :: TMVar a -> a -> CSTM r ()
putTMVar tm v = liftSTM $ S.putTMVar tm v
{-# INLINABLE putTMVar #-}

readTMVar :: TMVar a -> CSTM r a 
readTMVar tm = liftSTM $ S.readTMVar tm
{-# INLINABLE readTMVar #-}

tryReadTMVar :: TMVar a -> CSTM r (Maybe a)
tryReadTMVar tm = liftSTM $ S.tryReadTMVar tm
{-# INLINABLE tryReadTMVar #-}

swapTMVar :: TMVar a -> a -> CSTM r a
swapTMVar tm v = liftSTM $ S.swapTMVar tm v
{-# INLINABLE swapTMVar #-}

tryTakeTMVar :: TMVar a -> CSTM r (Maybe a)
tryTakeTMVar tm = liftSTM $ S.tryTakeTMVar tm
{-# INLINABLE tryTakeTMVar #-}

tryPutTMVar :: TMVar a -> a -> CSTM r Bool
tryPutTMVar tm v = liftSTM $ S.tryPutTMVar tm v
{-# INLINABLE tryPutTMVar #-}

isEmptyTMVar :: TMVar a -> CSTM r Bool
isEmptyTMVar tm = liftSTM $ S.isEmptyTMVar tm
{-# INLINABLE isEmptyTMVar #-}

--mkWeakTMVar :: TMVar a -> CIO () () -> CIO r (Weak (TMVar a))
--mkWeakTMVar tmv finalizer = liftIO $ S.mkWeakTMVar tmv (runCIO return finalizer)


-- | TQueue

newTQueue :: CSTM r (TQueue a)
newTQueue = liftSTM S.newTQueue
{-# INLINABLE newTQueue #-}

newTQueueCIO :: CIO r (TQueue a)
newTQueueCIO = liftIO S.newTQueueIO
{-# INLINABLE newTQueueCIO #-}

readTQueue :: TQueue a -> CSTM r a
readTQueue q = liftSTM $ S.readTQueue q
{-# INLINABLE readTQueue #-}

tryReadTQueue :: TQueue a -> CSTM r (Maybe a)
tryReadTQueue q = liftSTM $ S.tryReadTQueue q
{-# INLINABLE tryReadTQueue #-}

peekTQueue :: TQueue a -> CSTM r a
peekTQueue q = liftSTM $ S.peekTQueue q
{-# INLINABLE peekTQueue #-}

tryPeekTQueue :: TQueue a -> CSTM r (Maybe a)
tryPeekTQueue q = liftSTM $ S.tryPeekTQueue q
{-# INLINABLE tryPeekTQueue #-}

writeTQueue :: TQueue a -> a -> CSTM r ()
writeTQueue q v = liftSTM $ S.writeTQueue q v
{-# INLINABLE writeTQueue #-}

unGetTQueue :: TQueue a -> a -> CSTM r ()
unGetTQueue q v = liftSTM $ S.unGetTQueue q v
{-# INLINABLE unGetTQueue #-}

isEmptyTQueue :: TQueue a -> CSTM r Bool
isEmptyTQueue q = liftSTM $ S.isEmptyTQueue q
{-# INLINABLE isEmptyTQueue #-}
