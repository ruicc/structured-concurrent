module Control.Concurrent.Structured.Exception
    ( module Exception -- FIXME: Don't re-export directly.
    , E.SomeException(..)
    , catch, catch_, handle, handle_, onException, onException_
    , try, try_, mask, mask_
    , throwTo, throwCIO
    , bracket, bracket_, finally, finally_
    ) where


import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO)
import           Control.Monad.Trans.Cont (ContT(..), callCC)
import qualified Control.Exception as Exception hiding
        (catch, handle, onException, try, mask, mask_, throwTo, throwIO, bracket, finally)
import qualified Control.Exception as E
import qualified Control.Concurrent as C


catch :: E.Exception e => (a -> IO r') -> CIO r' a -> (e -> CIO r' a) -> CIO r r'
catch k action handler =
    callCC $ \ exit -> do
        ContT $ \ (k' :: a -> IO r) -> do -- IO
            -- @action@ runs with the last continuation @k@ which is feeded as an argument.
            -- It means that @action@ runs in the separated context from the current continuation @k'@,
            -- and memory leaks don't occur under proper programs.
            r' <- runCIO k action `E.catch` \ e -> runCIO k (handler e)
            runCIO k' (exit r')
{-# INLINABLE catch #-}

catch_ :: E.Exception e => CIO a a -> (e -> CIO a a) -> CIO r a
catch_ = catch (\a -> return a)
{-# INLINABLE catch_ #-}

handle :: E.Exception e => (a -> IO b) -> (e -> CIO b a) -> CIO b a -> CIO r b
handle k handler action = catch (\a -> k a) action handler
{-# INLINABLE handle #-}

handle_ :: E.Exception e => (e -> CIO a a) -> CIO a a -> CIO r a
handle_ = handle (\a -> return a)
{-# INLINABLE handle_ #-}

onException :: (a -> IO r') -> CIO r' a -> CIO r' b -> CIO r r'
onException k action handler =
    catch' action $ \ (e :: E.SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e
  where
    catch' = catch (\a -> k a)
{-# INLINABLE onException #-}

onException_ :: CIO a a -> CIO a b -> CIO r a
onException_ = onException (\a -> return a)
{-# INLINABLE onException_ #-}

try :: E.Exception e => (a -> IO r') -> CIO r' a -> CIO r (Either e r')
try k action =
    callCC $ \ exit -> do
        ContT $ \ k' -> do -- IO
                ei <- E.try $ runCIO k action
                runCIO k' (exit ei)
{-# INLINABLE try #-}

try_ :: E.Exception e => CIO a a -> CIO r (Either e a)
try_ = try (\a -> return a)
{-# INLINABLE try_ #-}

mask
    :: (a -> IO r') -- ^ Last action to feed to 2nd arg
    -> ((forall s b. CIO s b -> CIO s b) -> CIO r' a)
    -> CIO r r'
mask k userAction =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- IO
            r' <- E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
                let
                    restore :: forall r' b. CIO r' b -> CIO r' b
                    restore act = ContT $ \k'' -> unblock (runCIO k'' act)
                in
                    runCIO k (userAction restore)
            runCIO k' (exit r')
{-# INLINABLE mask #-}

mask_
    :: ((forall s b. CIO s b -> CIO s b) -> CIO r' r')
    -> CIO r r'
mask_ = mask (\a -> return a)
{-# INLINABLE mask_ #-}

throwTo :: E.Exception e => C.ThreadId -> e -> CIO r ()
throwTo tid e = liftIO $ C.throwTo tid e
{-# INLINABLE throwTo #-}

throwCIO :: E.Exception e => e -> CIO r a
throwCIO = liftIO . E.throwIO
{-# INLINABLE throwCIO #-}

bracket
    :: (c -> IO r')
    -> CIO r' a -- ^ before (typically, gaining a resource)
    -> (a -> CIO r' b) -- ^ after (release the resrouce)
    -> (a -> CIO r' c) -- ^ action (use the resrouce)
    -> CIO r r'
bracket k before after action =
    mask_ $ \restore -> do -- CIO r'
        a <- before
        r <- onException' (restore (action a)) (after a)
        _ <- after a
        return r
  where
    onException' = onException (\a -> k a)
{-# INLINABLE bracket #-}

bracket_
    :: CIO r' a -- ^ before (typically, gaining a resource)
    -> (a -> CIO r' b) -- ^ after (release the resrouce)
    -> (a -> CIO r' r') -- ^ action (use the resrouce)
    -> CIO r r'
bracket_ = bracket (\a -> return a)
{-# INLINABLE bracket_ #-}

finally
    :: (a -> IO r') -- ^ last action to feed
    -> CIO r' a -- ^ action
    -> CIO r' t -- ^ finalizer
    -> CIO r r'
finally k action finalizer =
    mask_ $ \restore -> do -- CIO r'
        r' <- onException' (restore action) finalizer
        _ <- finalizer
        return r'
  where
    onException' = onException (\a -> k a)
{-# INLINABLE finally #-}

finally_
    :: CIO a a -- ^ action
    -> CIO a t -- ^ finalizer
    -> CIO r a
finally_ = finally (\a -> return a)
{-# INLINABLE finally_ #-}
