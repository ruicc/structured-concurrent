module Control.Concurrent.Structured.Base where


myThreadId :: CIO r ThreadId
myThreadId = liftIO Conc.myThreadId
{-# INLINE myThreadId #-}

fork :: (a -> IO r') -> CIO r' a -> CIO r ThreadId
fork k action = liftIO $ Conc.forkIO (void $ runCIO (\a -> k a) action)
{-# INLINE fork #-}

fork_ :: CIO () () -> CIO r ThreadId
fork_ action = liftIO $ Conc.forkIO $ runCIO (\ () -> return ()) action
{-# INLINE fork_ #-}

forkFinally
    :: (a -> IO r')
    -> CIO r' a
    -> (Either SomeException r' -> CIO r'' r'')
    -> CIO r ThreadId
forkFinally k action finalizer =
    mask_ $ \ restore ->
        fork
            (\ ei -> runCIO return (finalizer ei))
            (try (\a -> k a) $ restore action)
{-# INLINE forkFinally #-}

forkFinally_
    :: CIO a a
    -> (Either SomeException a -> CIO r' r')
    -> CIO r ThreadId
forkFinally_ = forkFinally return
{-# INLINE forkFinally_ #-}

--forkWithUnmask :: ((forall a. CIO r a -> CIO r a) -> CIO r ()) -> CIO r ThreadId
--forkWithUnmask userAction = liftIO $ Conc.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) ->
--    let
--        unmask :: CIO r a -> CIO r a
--        unmask action = liftIO $ unmaskIO $ runConcurrent return action
--    in
--        runConcurrent return (userAction unmask)

killThread :: ThreadId -> CIO r ()
killThread = liftIO . Conc.killThread
{-# INLINE killThread #-}

threadDelay :: Int -> CIO r ()
threadDelay = liftIO . Conc.threadDelay
{-# INLINE threadDelay #-}
