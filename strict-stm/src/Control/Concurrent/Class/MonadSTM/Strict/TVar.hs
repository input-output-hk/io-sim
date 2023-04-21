{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeOperators      #-}

-- | This module corresponds to `Control.Concurrent.STM.TVar` in "stm" package
--
module Control.Concurrent.Class.MonadSTM.Strict.TVar
  ( -- * StrictTVar
    StrictTVar
  , LazyTVar
  , toLazyTVar
  , fromLazyTVar
  , castStrictTVar
  , newTVar
  , newTVarIO
  , newTVarWithInvariant
  , newTVarWithInvariantIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , stateTVar
  , swapTVar
  , check
    -- ** Low-level API
  , checkInvariant
    -- * MonadLabelSTM
  , labelTVar
  , labelTVarIO
    -- * MonadTraceSTM
  , traceTVar
  , traceTVarIO
  ) where

import qualified Control.Concurrent.Class.MonadSTM.TVar as Lazy
import           Control.Monad.Class.MonadSTM hiding (traceTVar, traceTVarIO)

import           GHC.Stack


type LazyTVar    m = Lazy.TVar m

#if CHECK_TVAR_INVARIANT
data StrictTVar m a = StrictTVar
   { invariant :: !(a -> Maybe String)
     -- ^ Invariant checked whenever updating the 'StrictTVar'.
   , tvar      :: !(LazyTVar m a)
   }
#else
newtype StrictTVar m a = StrictTVar
   { tvar      :: LazyTVar m a
   }
#endif

labelTVar :: MonadLabelledSTM m => StrictTVar m a -> String -> STM m ()
labelTVar StrictTVar { tvar } = Lazy.labelTVar tvar

labelTVarIO :: MonadLabelledSTM m => StrictTVar m a -> String -> m ()
labelTVarIO v = atomically . labelTVar v

traceTVar :: MonadTraceSTM m
          => proxy m
          -> StrictTVar m a
          -> (Maybe a -> a -> InspectMonad m TraceValue)
          -> STM m ()
traceTVar p StrictTVar {tvar} = Lazy.traceTVar p tvar

traceTVarIO :: MonadTraceSTM m
            => StrictTVar m a
            -> (Maybe a -> a -> InspectMonad m TraceValue)
            -> m ()
traceTVarIO StrictTVar {tvar} = Lazy.traceTVarIO tvar

castStrictTVar :: LazyTVar m ~ LazyTVar n
               => StrictTVar m a -> StrictTVar n a
castStrictTVar v@StrictTVar {tvar} =
    mkStrictTVar (getInvariant v) tvar

-- | Get the underlying @TVar@
--
-- Since we obviously cannot guarantee that updates to this 'LazyTVar' will be
-- strict, this should be used with caution.
toLazyTVar :: StrictTVar m a -> LazyTVar m a
toLazyTVar StrictTVar { tvar } = tvar

fromLazyTVar :: LazyTVar m a -> StrictTVar m a
fromLazyTVar tvar =
#if CHECK_TVAR_INVARIANT
  StrictTVar { invariant = const Nothing
             , tvar
             }
#else
  StrictTVar { tvar }
#endif

newTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
newTVar !a = (\tvar -> mkStrictTVar (const Nothing) tvar)
         <$> Lazy.newTVar a

newTVarIO :: MonadSTM m => a -> m (StrictTVar m a)
newTVarIO = newTVarWithInvariantIO (const Nothing)

newTVarWithInvariant :: (MonadSTM m, HasCallStack)
                     => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                     -> a
                     -> STM m (StrictTVar m a)
newTVarWithInvariant  invariant !a =
        checkInvariant (invariant a) $
        (\tvar -> mkStrictTVar invariant tvar)
    <$> Lazy.newTVar a

newTVarWithInvariantIO :: (MonadSTM m, HasCallStack)
                       => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                       -> a
                       -> m (StrictTVar m a)
newTVarWithInvariantIO  invariant !a =
        checkInvariant (invariant a) $
        (\tvar -> mkStrictTVar invariant tvar)
    <$> Lazy.newTVarIO a

readTVar :: MonadSTM m => StrictTVar m a -> STM m a
readTVar StrictTVar { tvar } = Lazy.readTVar tvar

readTVarIO :: MonadSTM m => StrictTVar m a -> m a
readTVarIO StrictTVar { tvar } = Lazy.readTVarIO tvar

writeTVar :: (MonadSTM m, HasCallStack) => StrictTVar m a -> a -> STM m ()
writeTVar v !a =
    checkInvariant (getInvariant v a) $
    Lazy.writeTVar (tvar v) a

modifyTVar :: MonadSTM m => StrictTVar m a -> (a -> a) -> STM m ()
modifyTVar v f = readTVar v >>= writeTVar v . f

stateTVar :: MonadSTM m => StrictTVar m s -> (s -> (a, s)) -> STM m a
stateTVar v f = do
    a <- readTVar v
    let (b, a') = f a
    writeTVar v a'
    return b

swapTVar :: MonadSTM m => StrictTVar m a -> a -> STM m a
swapTVar v a' = do
    a <- readTVar v
    writeTVar v a'
    return a


{-------------------------------------------------------------------------------
  Dealing with invariants
-------------------------------------------------------------------------------}

getInvariant :: StrictTVar m a -> a -> Maybe String
mkStrictTVar :: (a -> Maybe String) -> Lazy.TVar m a -> StrictTVar m a

-- | Check invariant (if enabled) before continuing
--
-- @checkInvariant mErr x@ is equal to @x@ if @mErr == Nothing@, and throws
-- an error @err@ if @mErr == Just err@.
--
-- This is exported so that other code that wants to conditionally check
-- invariants can reuse the same logic, rather than having to introduce new
-- per-package flags.
checkInvariant :: HasCallStack => Maybe String -> a -> a

#if CHECK_TVAR_INVARIANT
getInvariant StrictTVar {invariant} = invariant
mkStrictTVar invariant  tvar = StrictTVar {invariant, tvar}

checkInvariant Nothing    k = k
checkInvariant (Just err) _ = error $ "Invariant violation: " ++ err
#else
getInvariant _               = \_ -> Nothing
mkStrictTVar _invariant tvar = StrictTVar {tvar}

checkInvariant _err       k  = k
#endif
