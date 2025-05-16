{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Concurrent.Class.MonadMVar
  ( MonadMVar (..)
    -- * non-standard extensions
  , MonadInspectMVar (..)
  , MonadTraceMVar (..)
  , MonadLabelledMVar (..)
  ) where

import Control.Concurrent.MVar qualified as IO
import Control.Monad.Class.MonadThrow

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)

import Control.Concurrent.Class.MonadSTM (TraceValue)
import Data.Kind (Type)


class Monad m => MonadMVar m where
  {-# MINIMAL newEmptyMVar,
              takeMVar, tryTakeMVar,
              putMVar,  tryPutMVar,
              readMVar, tryReadMVar,
              isEmptyMVar #-}

  type MVar m :: Type -> Type

  -- | See 'IO.newEmptyMVar'.
  newEmptyMVar      :: m (MVar m a)
  -- | See 'IO.takeMVar'.
  takeMVar          :: MVar m a -> m a
  -- | See 'IO.putMVar'.
  putMVar           :: MVar m a -> a -> m ()
  -- | See 'IO.tryTakeMVar'.
  tryTakeMVar       :: MVar m a -> m (Maybe a)
  -- | See 'IO.tryPutMVar'.
  tryPutMVar        :: MVar m a -> a -> m Bool
  -- | See 'IO.isEmptyMVar'.
  isEmptyMVar       :: MVar m a -> m Bool

  -- methods with a default implementation
  -- | See 'IO.newMVar'.
  newMVar           :: a -> m (MVar m a)
  -- | See 'IO.readMVar'.
  readMVar          :: MVar m a -> m a
  -- | See 'IO.tryReadMVar'.
  tryReadMVar       :: MVar m a -> m (Maybe a)
  -- | See 'IO.swapMVar'.
  swapMVar          :: MVar m a -> a -> m a
  -- | See 'IO.withMVar'.
  withMVar          :: MVar m a -> (a -> m b) -> m b
  -- | See 'IO.withMVarMasked'.
  withMVarMasked    :: MVar m a -> (a -> m b) -> m b
  -- | See 'IO.modifyMVar_'.
  modifyMVar_       :: MVar m a -> (a -> m a) -> m ()
  -- | See 'IO.modifyMVar'.
  modifyMVar        :: MVar m a -> (a -> m (a, b)) -> m b
  -- | See 'IO.modifyMVarMasked_'.
  modifyMVarMasked_ :: MVar m a -> (a -> m a) -> m ()
  -- | See 'IO.modifyMVarMasked'.
  modifyMVarMasked  :: MVar m a -> (a -> m (a,b)) -> m b

  default newMVar :: a -> m (MVar m a)
  newMVar a = do
    v <- newEmptyMVar
    putMVar v a
    return v
  {-# INLINE newMVar #-}

  default swapMVar :: MonadMask m => MVar m a -> a -> m a
  swapMVar mvar new =
    mask_ $ do
      old <- takeMVar mvar
      putMVar mvar new
      return old
  {-# INLINE swapMVar #-}

  default withMVar :: MonadMask m => MVar m a -> (a -> m b) -> m b
  withMVar m io =
    mask $ \restore -> do
      a <- takeMVar m
      b <- restore (io a) `onException` putMVar m a
      putMVar m a
      return b
  {-# INLINE withMVar #-}

  default withMVarMasked :: MonadMask m => MVar m a -> (a -> m b) -> m b
  withMVarMasked m io =
    mask_ $ do
      a <- takeMVar m
      b <- io a `onException` putMVar m a
      putMVar m a
      return b
  {-# INLINE withMVarMasked #-}

  default modifyMVar_ :: MonadMask m => MVar m a -> (a -> m a) -> m ()
  modifyMVar_ m io =
    mask $ \restore -> do
      a  <- takeMVar m
      a' <- restore (io a) `onException` putMVar m a
      putMVar m a'
  {-# INLINE modifyMVar_ #-}

  default modifyMVar :: (MonadMask m, MonadEvaluate m)
                     => MVar m a -> (a -> m (a,b)) -> m b
  modifyMVar m io =
    mask $ \restore -> do
      a      <- takeMVar m
      (a',b) <- restore (io a >>= evaluate) `onException` putMVar m a
      putMVar m a'
      return b
  {-# INLINE modifyMVar #-}

  default modifyMVarMasked_ :: MonadMask m => MVar m a -> (a -> m a) -> m ()
  modifyMVarMasked_ m io =
    mask_ $ do
      a  <- takeMVar m
      a' <- io a `onException` putMVar m a
      putMVar m a'
  {-# INLINE modifyMVarMasked_ #-}

  default modifyMVarMasked :: (MonadMask m, MonadEvaluate m)
                           => MVar m a -> (a -> m (a,b)) -> m b
  modifyMVarMasked m io =
    mask_ $ do
      a      <- takeMVar m
      (a',b) <- (io a >>= evaluate) `onException` putMVar m a
      putMVar m a'
      return b
  {-# INLINE modifyMVarMasked #-}

--
-- IO instance
--

instance MonadMVar IO where
    type MVar IO      = IO.MVar
    newEmptyMVar      = IO.newEmptyMVar
    newMVar           = IO.newMVar
    takeMVar          = IO.takeMVar
    putMVar           = IO.putMVar
    readMVar          = IO.readMVar
    swapMVar          = IO.swapMVar
    tryTakeMVar       = IO.tryTakeMVar
    tryPutMVar        = IO.tryPutMVar
    tryReadMVar       = IO.tryReadMVar
    isEmptyMVar       = IO.isEmptyMVar
    withMVar          = IO.withMVar
    withMVarMasked    = IO.withMVarMasked
    modifyMVar_       = IO.modifyMVar_
    modifyMVar        = IO.modifyMVar
    modifyMVarMasked_ = IO.modifyMVarMasked_
    modifyMVarMasked  = IO.modifyMVarMasked

--
-- ReaderT instance
--

newtype WrappedMVar r (m :: Type -> Type) a = WrappedMVar { unwrapMVar :: MVar m a }

instance ( MonadMask m
         , MonadMVar m
         ) => MonadMVar (ReaderT r m) where
    type MVar (ReaderT r m) = WrappedMVar r m
    newEmptyMVar = WrappedMVar <$> lift newEmptyMVar
    newMVar      = fmap WrappedMVar . lift . newMVar
    takeMVar     = lift .   takeMVar    . unwrapMVar
    putMVar      = lift .: (putMVar     . unwrapMVar)
    readMVar     = lift .   readMVar    . unwrapMVar
    tryReadMVar  = lift .   tryReadMVar . unwrapMVar
    swapMVar     = lift .: (swapMVar    . unwrapMVar)
    tryTakeMVar  = lift .   tryTakeMVar . unwrapMVar
    tryPutMVar   = lift .: (tryPutMVar  . unwrapMVar)
    isEmptyMVar  = lift .   isEmptyMVar . unwrapMVar
    withMVar (WrappedMVar v) f = ReaderT $ \r ->
      withMVar v (\a -> runReaderT (f a) r)
    withMVarMasked (WrappedMVar v) f = ReaderT $ \r ->
      withMVarMasked v (\a -> runReaderT (f a) r)
    modifyMVar_ (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVar_ v (\a -> runReaderT (f a) r)
    modifyMVar (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVar v (\a -> runReaderT (f a) r)
    modifyMVarMasked_ (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVarMasked_ v (\a -> runReaderT (f a) r)
    modifyMVarMasked (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVarMasked v (\a -> runReaderT (f a) r)

--
-- MonadInspectMVar
--

-- | This type class is intended for
-- ['io-sim'](https://hackage.haskell.org/package/io-sim), where one might want
-- to access an 'MVar' in the underlying 'ST' monad.
class (MonadMVar m, Monad (InspectMVarMonad m)) => MonadInspectMVar m where
  type InspectMVarMonad m :: Type -> Type
  -- | Return the value of an 'MVar' as an 'InspectMVarMonad' computation. Can
  -- be 'Nothing' if the 'MVar' is empty.
  inspectMVar :: proxy m -> MVar m a -> InspectMVarMonad m (Maybe a)

instance MonadInspectMVar IO where
  type InspectMVarMonad IO = IO
  inspectMVar _ = tryReadMVar

class MonadTraceMVar m where
  traceMVarIO :: proxy
              -> MVar m a
              -> (Maybe (Maybe a) -> Maybe a -> InspectMVarMonad m TraceValue)
              -> m ()

instance MonadTraceMVar IO where
  traceMVarIO = \_ _ _ -> pure ()

-- | Labelled `MVar`s
--
-- The `IO` instances is no-op, the `IOSim` instance enhances simulation trace.
-- This is very useful when analysing low lever concurrency issues (e.g.
-- deadlocks, livelocks etc).
class MonadMVar m
   => MonadLabelledMVar m where
  -- | Name an `MVar`
  labelMVar :: MVar m a -> String -> m ()

instance MonadLabelledMVar IO where
  labelMVar = \_ _ -> pure ()
--
-- Utilities
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
