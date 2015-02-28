module Control.Concurrent.Structured.Exception
    ( module Exception -- FIXME: Don't re-export directly.
    , E.SomeException(..)
    , catch, catch_, handle, handle_, onException, onException_
    , try, try_, mask, mask_
    , throwTo, throwCIO
    , bracket, bracket_, finally, finally_
    ) where

import           GHC.TypeLits

import           Control.Monad.Concurrent.Structured (CIO, runCIO, liftIO, IO')
import           Control.Monad.Trans.Cont (ContT(..), callCC)
import qualified Control.Exception as Exception hiding
        (catch, handle, onException, try, mask, mask_, throwTo, throwIO, bracket, finally)
import qualified Control.Exception as E
import qualified Control.Concurrent as C


catch :: E.Exception e => (a -> IO' (n + 1) r') -> CIO (n + 1) r' a -> (e -> CIO (n + 1) r' a) -> CIO n r r'
catch k action handler =
    callCC $ \ exit -> do
        ContT $ \ (k' :: a -> IO r) -> do -- IO
                -- Makes @action@ run with the last continuation @k@ which is feeded as an argument.
                -- It means that @action@ is in the separated context from the current continuation @k'@,
                -- and memory leaks don't occur under proper progurams.
                r' <- runCIO k action `E.catch` \ e -> runCIO k (handler e)
                runCIO k' (exit r')
{-# INLINE catch #-}

catch_ :: E.Exception e => CIO (n + 1) a a -> (e -> CIO (n + 1) a a) -> CIO n r a
catch_ = catch (\a -> return a)
{-# INLINE catch_ #-}

handle :: E.Exception e => (a -> IO' (n + 1) b) -> (e -> CIO (n + 1) b a) -> CIO (n + 1) b a -> CIO n r b
handle k handler action = catch (\a -> k a) action handler
{-# INLINE handle #-}

handle_ :: E.Exception e => (e -> CIO (n + 1) a a) -> CIO (n + 1) a a -> CIO n r a
handle_ = handle (\a -> return a)
{-# INLINE handle_ #-}

onException :: (a -> IO' (n + 1) r') -> CIO (n + 1) r' a -> CIO (n + 1) r' b -> CIO n r r'
onException k action handler =
    catch' action $ \ (e :: E.SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e
  where
    catch' = catch (\a -> k a)
{-# INLINE onException #-}

onException_ :: CIO (n + 1) a a -> CIO (n + 1) a b -> CIO n r a
onException_ = onException (\a -> return a)
{-# INLINE onException_ #-}

try :: E.Exception e => (a -> IO' (n + 1) r') -> CIO (n + 1) r' a -> CIO n r (Either e r')
try k action =
    callCC $ \ exit -> do
        ContT $ \ k' -> do -- IO
                ei <- E.try $ runCIO k action
                runCIO k' (exit ei)
{-# INLINE try #-}

try_ :: E.Exception e => CIO (n + 1) a a -> CIO n r (Either e a)
try_ = try (\a -> return a)
{-# INLINE try_ #-}

mask
    :: (a -> IO' (n + 1) r') -- ^ Last action to feed to 2nd arg
    -> ((forall s b. CIO (n + 1) s b -> CIO (n + 1) s b) -> CIO (n + 1) r' a)
    -> CIO n r r'
mask k userAction =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- IO
            r' <- E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
                let
                    restore :: forall r a n. CIO (n + 1) r a -> CIO (n + 1) r a
                    restore act = ContT $ \k'' -> unblock (runCIO k'' act)
                in
                    runCIO k (userAction restore)
            runCIO k' (exit r')
{-# INLINE mask #-}

mask_
    :: ((forall s b. CIO (n + 1) s b -> CIO (n + 1) s b) -> CIO (n + 1) a a)
    -> CIO n r a
mask_ = mask (\a -> return a)
{-# INLINE mask_ #-}

throwTo :: E.Exception e => C.ThreadId -> e -> CIO n r ()
throwTo tid e = liftIO $ C.throwTo tid e
{-# INLINE throwTo #-}

throwCIO :: E.Exception e => e -> CIO n r a
throwCIO = liftIO . E.throwIO
{-# INLINE throwCIO #-}

bracket
    :: (c -> IO' (n + 1) r')
    -> CIO (n + 1) r' a -- ^ before (typically, gaining a resource)
    -> (a -> CIO (n + 1) r' b) -- ^ after (release the resrouce)
    -> (a -> CIO (n + 1) r' c) -- ^ action (use the resrouce)
    -> CIO n r r'
bracket k before after action =
    mask_ $ \restore -> do -- CIO r'
        a <- before
        r <- onException' (restore (action a)) (after a)
        _ <- after a
        return r
  where
    onException' = onException (\a -> k a)
{-# INLINE bracket #-}

bracket_
    :: CIO (n + 1) r' a -- ^ before (typically, gaining a resource)
    -> (a -> CIO (n + 1) r' b) -- ^ after (release the resrouce)
    -> (a -> CIO (n + 1) r' r') -- ^ action (use the resrouce)
    -> CIO n r r'
bracket_ = bracket (\a -> return a)
{-# INLINE bracket_ #-}

finally
    :: (a -> IO' (n + 1) r') -- ^ last action to feed
    -> CIO (n + 1) r' a -- ^ action
    -> CIO (n + 1) r' t -- ^ finalizer
    -> CIO n r r'
finally k action finalizer =
    mask_ $ \restore -> do -- CIO r'
        r' <- onException' (restore action) finalizer
        _ <- finalizer
        return r'
  where
    onException' = onException (\a -> k a)
{-# INLINE finally #-}

finally_
    :: CIO (n + 1) a a -- ^ action
    -> CIO (n + 1) a t -- ^ finalizer
    -> CIO n r a
finally_ = finally (\a -> return a)
{-# INLINE finally_ #-}
