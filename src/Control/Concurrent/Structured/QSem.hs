module Control.Concurrent.Structured.QSem
    ( QS.QSem
    , newQSem, waitQSem, signalQSem
    ) where

import           Control.Monad.Concurrent.Structured (CIO, liftIO)
import qualified Control.Concurrent.QSem as QS

type QSem = QS.QSem


newQSem :: Int -> CIO r QSem
newQSem n = liftIO $ QS.newQSem n

waitQSem :: QSem -> CIO r ()
waitQSem q = liftIO $ QS.waitQSem q

signalQSem :: QSem -> CIO r ()
signalQSem q = liftIO $ QS.signalQSem q
