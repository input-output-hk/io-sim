{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Monad.Class.MonadMVar
  ( MonadMVar (..)
  ) where

import qualified Control.Concurrent.MVar as IO
import           Control.Monad.Class.MonadThrow

import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans (lift)

import           Data.Kind (Type)


class Monad m => MonadMVar m where
  {-# MINIMAL newEmptyMVar,
              takeMVar, tryTakeMVar,
              putMVar,  tryPutMVar,
              readMVar, tryReadMVar,
              isEmptyMVar #-}

  type MVar m :: Type -> Type

  newEmptyMVar      :: m (MVar m a)
  takeMVar          :: MVar m a -> m a
  putMVar           :: MVar m a -> a -> m ()
  tryTakeMVar       :: MVar m a -> m (Maybe a)
  tryPutMVar        :: MVar m a -> a -> m Bool
  readMVar          :: MVar m a -> m a
  tryReadMVar       :: MVar m a -> m (Maybe a)
  isEmptyMVar       :: MVar m a -> m Bool

  -- methods with a default implementation
  newMVar           :: a -> m (MVar m a)
  swapMVar          :: MVar m a -> a -> m a
  withMVar          :: MVar m a -> (a -> m b) -> m b
  withMVarMasked    :: MVar m a -> (a -> m b) -> m b
  modifyMVar_       :: MVar m a -> (a -> m a) -> m ()
  modifyMVar        :: MVar m a -> (a -> m (a, b)) -> m b
  modifyMVarMasked_ :: MVar m a -> (a -> m a) -> m ()
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
         , MonadEvaluate m
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
-- Utilities
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
