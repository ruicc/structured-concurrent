module Control.Concurrent.Structured.MVar
    ( MV.MVar
    , newEmptyMVar, newMVar
    , takeMVar, putMVar, readMVar, swapMVar
    , tryTakeMVar, tryPutMVar, isEmptyMVar
    ) where

import           Control.Monad.Concurrent.Structured (CIO, liftIO)
import qualified Control.Concurrent.MVar as MV

type MVar = MV.MVar

newEmptyMVar :: CIO n r (MVar a)
newEmptyMVar = liftIO MV.newEmptyMVar

newMVar :: a -> CIO n r (MVar a)
newMVar v = liftIO $ MV.newMVar v

takeMVar :: MVar a -> CIO n r a
takeMVar mv = liftIO $ MV.takeMVar mv

putMVar :: MVar a -> a -> CIO n r ()
putMVar mv v = liftIO $ MV.putMVar mv v

readMVar :: MVar a -> CIO n r a
readMVar mv = liftIO $ MV.readMVar mv

swapMVar :: MVar a -> a -> CIO n r a
swapMVar mv v = liftIO $ MV.swapMVar mv v

tryTakeMVar :: MVar a -> CIO n r (Maybe a)
tryTakeMVar mv = liftIO $ MV.tryTakeMVar mv

tryPutMVar :: MVar a -> a -> CIO n r Bool
tryPutMVar mv v = liftIO $ MV.tryPutMVar mv v

isEmptyMVar :: MVar a -> CIO n r Bool
isEmptyMVar mv = liftIO $ MV.isEmptyMVar mv
