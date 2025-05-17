-- | This module corresponds to "Control.Concurrent.MVar" in the @base@ package.
module Control.Concurrent.Class.MonadMVar.Strict
  ( -- * StrictMVar
    StrictMVar
  , LazyMVar
  , castStrictMVar
  , toLazyMVar
  , fromLazyMVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar_
  , modifyMVar
  , modifyMVarMasked_
  , modifyMVarMasked
  , tryReadMVar
  , labelMVar
    -- * Re-exports
  , MonadMVar
  ) where

import Control.Concurrent.Class.MonadMVar (MonadLabelledMVar, MonadMVar)
import Control.Concurrent.Class.MonadMVar qualified as Lazy

--
-- StrictMVar
--

type LazyMVar m = Lazy.MVar m

newtype StrictMVar m a = StrictMVar {
    mvar      :: LazyMVar m a
  }

castStrictMVar :: LazyMVar m ~ LazyMVar n
               => StrictMVar m a -> StrictMVar n a
castStrictMVar v = StrictMVar (mvar v)

-- | Get the underlying @MVar@
--
-- Since we obviously cannot guarantee that updates to this 'LazyMVar' will be
-- strict, this should be used with caution.
toLazyMVar :: StrictMVar m a -> LazyMVar m a
toLazyMVar = mvar

-- | Create a 'StrictMVar' from a 'LazyMVar'
--
-- It is not guaranteed that the 'LazyMVar' contains a value that is in WHNF, so
-- there is no guarantee that the resulting 'StrictMVar' contains a value that
-- is in WHNF. This should be used with caution.
fromLazyMVar :: Lazy.MVar m a -> StrictMVar m a
fromLazyMVar = StrictMVar

labelMVar :: MonadLabelledMVar m => StrictMVar m a -> String -> m ()
labelMVar (StrictMVar m) = Lazy.labelMVar m

newEmptyMVar :: MonadMVar m => m (StrictMVar m a)
newEmptyMVar = fromLazyMVar <$> Lazy.newEmptyMVar

newMVar :: MonadMVar m => a -> m (StrictMVar m a)
newMVar !a = fromLazyMVar <$> Lazy.newMVar a

takeMVar :: MonadMVar m => StrictMVar m a -> m a
takeMVar = Lazy.takeMVar . mvar

putMVar :: MonadMVar m => StrictMVar m a -> a -> m ()
putMVar v !a = Lazy.putMVar (mvar v) a

readMVar :: MonadMVar m => StrictMVar m a -> m a
readMVar v = Lazy.readMVar (mvar v)

swapMVar :: MonadMVar m => StrictMVar m a -> a -> m a
swapMVar v !a = Lazy.swapMVar (mvar v) a

tryTakeMVar :: MonadMVar m => StrictMVar m a -> m (Maybe a)
tryTakeMVar v = Lazy.tryTakeMVar (mvar v)

tryPutMVar :: MonadMVar m => StrictMVar m a -> a -> m Bool
tryPutMVar v !a = Lazy.tryPutMVar (mvar v) a

isEmptyMVar :: MonadMVar m => StrictMVar m a -> m Bool
isEmptyMVar v = Lazy.isEmptyMVar (mvar v)

withMVar :: MonadMVar m => StrictMVar m a -> (a -> m b) -> m b
withMVar v = Lazy.withMVar (mvar v)

withMVarMasked :: MonadMVar m => StrictMVar m a -> (a -> m b) -> m b
withMVarMasked v = Lazy.withMVarMasked (mvar v)

modifyMVar_ :: MonadMVar m => StrictMVar m a -> (a -> m a) -> m ()
modifyMVar_ v io = Lazy.modifyMVar_ (mvar v) io'
  where
    io' a = do
      !a' <- io a
      pure a'

modifyMVar :: MonadMVar m => StrictMVar m a -> (a -> m (a, b)) -> m b
modifyMVar v io = Lazy.modifyMVar (mvar v) io'
  where
    io' a = do
      (!a', b) <- io a
      pure (a', b)

modifyMVarMasked_ :: MonadMVar m => StrictMVar m a -> (a -> m a) -> m ()
modifyMVarMasked_ v io = Lazy.modifyMVarMasked_ (mvar v) io'
  where
    io' a = do
      !a' <- io a
      pure a'

modifyMVarMasked :: MonadMVar m => StrictMVar m a -> (a -> m (a,b)) -> m b
modifyMVarMasked v io = Lazy.modifyMVarMasked (mvar v) io'
  where
    io' a = do
      (!a', b) <- io a
      pure (a', b)

tryReadMVar :: MonadMVar m => StrictMVar m a -> m (Maybe a)
tryReadMVar v = Lazy.tryReadMVar (mvar v)
