module Control.Concurrent.Structured.Chan
    ( Ch.Chan
    , newChan
    , writeChan, readChan, dupChan
    , getChanContents, writeList2Chan
    ) where

import           Control.Monad.Concurrent.Structured (CIO, liftIO)
import qualified Control.Concurrent.Chan as Ch

type Chan = Ch.Chan


newChan :: CIO r (Chan a)
newChan = liftIO Ch.newChan

writeChan :: Chan a -> a -> CIO r ()
writeChan c v = liftIO $ Ch.writeChan c v

readChan :: Chan a -> CIO r a
readChan c = liftIO $ Ch.readChan c

dupChan :: Chan a -> CIO r (Chan a)
dupChan c = liftIO $ Ch.dupChan c

getChanContents :: Chan a -> CIO r [a]
getChanContents c = liftIO $ Ch.getChanContents c

writeList2Chan :: Chan a -> [a] -> CIO r ()
writeList2Chan c vs = liftIO $ Ch.writeList2Chan c vs
