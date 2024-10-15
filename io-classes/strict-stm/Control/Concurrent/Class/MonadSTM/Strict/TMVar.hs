{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeOperators      #-}

-- | This module corresponds to `Control.Concurrent.STM.TMVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TMVar
  ( -- * StrictTMVar
    StrictTMVar
  , LazyTMVar
  , toLazyTMVar
  , fromLazyTMVar
  , castStrictTMVar
  , newTMVar
  , newEmptyTMVar
  , newTMVarIO
  , newEmptyTMVarIO
  , takeTMVar
  , tryTakeTMVar
  , putTMVar
  , tryPutTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , writeTMVar
  , isEmptyTMVar
  , withTMVar
  , withTMVarAnd
    -- * MonadLabelledSTM
  , labelTMVar
  , labelTMVarIO
    -- * MonadTraceSTM
  , traceTMVar
  , traceTMVarIO
  , traceTMVarShow
  , traceTMVarShowIO
  ) where


import Control.Concurrent.Class.MonadSTM.TMVar qualified as Lazy
import Control.Monad.Class.MonadSTM hiding (traceTMVar, traceTMVarIO)
import Control.Monad.Class.MonadThrow

type LazyTMVar   m = Lazy.TMVar m

-- | 'TMVar' that keeps its value in WHNF at all times
newtype StrictTMVar m a = StrictTMVar { toLazyTMVar :: LazyTMVar m a }

fromLazyTMVar :: LazyTMVar m a -> StrictTMVar m a
fromLazyTMVar = StrictTMVar

labelTMVar :: MonadLabelledSTM m => StrictTMVar m a -> String -> STM m ()
labelTMVar (StrictTMVar tvar) = Lazy.labelTMVar tvar

labelTMVarIO :: MonadLabelledSTM m => StrictTMVar m a -> String -> m ()
labelTMVarIO v = atomically . labelTMVar v

traceTMVar :: MonadTraceSTM m
           => proxy m
           -> StrictTMVar m a
           -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
           -> STM m ()
traceTMVar p (StrictTMVar var) = Lazy.traceTMVar p var

traceTMVarShow :: (MonadTraceSTM m, Show a)
               => proxy m
               -> StrictTMVar m a
               -> STM m ()
traceTMVarShow p tmvar =
  traceTMVar p tmvar (\pv v -> pure $ TraceString $ case (pv, v) of
          (Nothing, Nothing) -> "Created empty"
          (Nothing, Just st') -> "Created full: " <> show st'
          (Just Nothing, Just st') -> "Put: " <> show st'
          (Just Nothing, Nothing) -> "Remains empty"
          (Just Just{}, Nothing) -> "Take"
          (Just (Just st'), Just st'') -> "Modified: " <> show st' <> " -> " <> show st''
      )

traceTMVarIO :: MonadTraceSTM m
             => StrictTMVar m a
             -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
             -> m ()
traceTMVarIO (StrictTMVar var) = Lazy.traceTMVarIO var

traceTMVarShowIO :: (Show a, MonadTraceSTM m)
                 => StrictTMVar m a
                 -> m ()
traceTMVarShowIO tmvar =
  traceTMVarIO tmvar (\pv v -> pure $ TraceString $ case (pv, v) of
          (Nothing, Nothing) -> "Created empty"
          (Nothing, Just st') -> "Created full: " <> show st'
          (Just Nothing, Just st') -> "Put: " <> show st'
          (Just Nothing, Nothing) -> "Remains empty"
          (Just Just{}, Nothing) -> "Take"
          (Just (Just st'), Just st'') -> "Modified: " <> show st' <> " -> " <> show st''
      )

castStrictTMVar :: LazyTMVar m ~ LazyTMVar n
                => StrictTMVar m a -> StrictTMVar n a
castStrictTMVar (StrictTMVar var) = StrictTMVar var

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = StrictTMVar <$> Lazy.newTMVar a

newTMVarIO :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarIO !a = StrictTMVar <$> Lazy.newTMVarIO a

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = StrictTMVar <$> Lazy.newEmptyTMVar

newEmptyTMVarIO :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarIO = StrictTMVar <$> Lazy.newEmptyTMVarIO

takeTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
takeTMVar (StrictTMVar tmvar) = Lazy.takeTMVar tmvar

tryTakeTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryTakeTMVar (StrictTMVar tmvar) = Lazy.tryTakeTMVar tmvar

putTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m ()
putTMVar (StrictTMVar tmvar) !a = Lazy.putTMVar tmvar a

tryPutTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m Bool
tryPutTMVar (StrictTMVar tmvar) !a = Lazy.tryPutTMVar tmvar a

readTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
readTMVar (StrictTMVar tmvar) = Lazy.readTMVar tmvar

tryReadTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryReadTMVar (StrictTMVar tmvar) = Lazy.tryReadTMVar tmvar

swapTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m a
swapTMVar (StrictTMVar tmvar) !a = Lazy.swapTMVar tmvar a

writeTMVar :: MonadSTM m => StrictTMVar m a -> a -> STM m ()
writeTMVar (StrictTMVar tmvar) !a = Lazy.writeTMVar tmvar a

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar (StrictTMVar tmvar) = Lazy.isEmptyTMVar tmvar

withTMVar :: (MonadSTM m, MonadCatch m)
          => StrictTMVar m a
          -> (a -> m (c, a))
          -> m c
withTMVar (StrictTMVar tmvar) f =
  Lazy.withTMVar tmvar (\x -> do
                           !(!c, !a) <- f x
                           pure $! (c, a)
                       )

withTMVarAnd :: (MonadSTM m, MonadCatch m)
             => StrictTMVar m a
             -> (a -> STM m b)
             -> (a -> b -> m (c, a))
             -> m c
withTMVarAnd (StrictTMVar tmvar) f g =
  Lazy.withTMVarAnd tmvar f (\x y -> do
                                !(!c, !a) <- g x y
                                pure $! (c, a)
                            )
