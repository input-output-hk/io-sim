{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- to preserve 'HasCallstack' constraint on 'checkInvariant'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Class.MonadSTM.Strict
  ( module X
  , LazyTVar
  , LazyTMVar
  , LazyTQueue
  , LazyTBQueue
  , LazyTArray
    -- * 'StrictTVar'
  , StrictTVar
  , labelTVar
  , labelTVarIO
  , traceTVar
  , traceTVarIO
  , castStrictTVar
  , toLazyTVar
  , fromLazyTVar
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
    -- * 'StrictTMVar'
  , StrictTMVar
  , labelTMVar
  , labelTMVarIO
  , traceTMVar
  , traceTMVarIO
  , castStrictTMVar
  , toLazyTMVar
  , fromLazyTMVar
  , newTMVar
  , newTMVarIO
  , newEmptyTMVar
  , newEmptyTMVarIO
  , takeTMVar
  , tryTakeTMVar
  , putTMVar
  , tryPutTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , isEmptyTMVar
    -- * 'StrictTQueue'
  , StrictTQueue
  , labelTQueue
  , labelTQueueIO
  , traceTQueue
  , traceTQueueIO
  , toLazyTQueue
  , fromLazyTQueue
  , newTQueue
  , newTQueueIO
  , readTQueue
  , tryReadTQueue
  , peekTQueue
  , tryPeekTQueue
  , writeTQueue
  , isEmptyTQueue
  , unGetTQueue
    -- * 'StrictTBQueue'
  , StrictTBQueue
  , labelTBQueue
  , labelTBQueueIO
  , traceTBQueue
  , traceTBQueueIO
  , toLazyTBQueue
  , fromLazyTBQueue
  , newTBQueue
  , newTBQueueIO
  , readTBQueue
  , tryReadTBQueue
  , peekTBQueue
  , tryPeekTBQueue
  , writeTBQueue
  , lengthTBQueue
  , isEmptyTBQueue
  , isFullTBQueue
  , unGetTBQueue
    -- * 'StrictTArray'
  , StrictTArray
  , toLazyTArray
  , fromLazyTArray
    -- * 'StrictTChan'
  , StrictTChan
  , toLazyTChan
  , fromLazyTChan
  , newTChan
  , newBroadcastTChan
  , writeTChan
  , readTChan
  , tryReadTChan
  , peekTChan
  , tryPeekTChan
  , dupTChan
  , unGetTChan
  , isEmptyTChan
  , cloneTChan
    -- ** Low-level API
  , checkInvariant
    -- * Deprecated API
  , updateTVar
  , newTVarM
  , newTVarWithInvariantM
  , newTMVarM
  , newEmptyTMVarM
  ) where

import           Control.Monad.Class.MonadSTM as X hiding (LazyTMVar, LazyTVar,
                     TMVar, TVar, cloneTChan, dupTChan, isEmptyTBQueue,
                     isEmptyTChan, isEmptyTMVar, isEmptyTQueue, isFullTBQueue,
                     labelTBQueue, labelTBQueueIO, labelTMVar, labelTMVarIO,
                     labelTQueue, labelTQueueIO, labelTVar, labelTVarIO,
                     lengthTBQueue, modifyTVar, newBroadcastTChan,
                     newEmptyTMVar, newEmptyTMVarIO, newEmptyTMVarM, newTBQueue,
                     newTBQueueIO, newTChan, newTMVar, newTMVarIO, newTMVarM,
                     newTQueue, newTQueueIO, newTVar, newTVarIO, newTVarM,
                     peekTBQueue, peekTChan, peekTQueue, putTMVar, readTBQueue,
                     readTChan, readTMVar, readTQueue, readTVar, readTVarIO,
                     stateTVar, swapTMVar, swapTVar, takeTMVar, traceTBQueue,
                     traceTBQueueIO, traceTMVar, traceTMVarIO, traceTQueue,
                     traceTQueueIO, traceTVar, traceTVarIO, tryPeekTBQueue,
                     tryPeekTChan, tryPeekTQueue, tryPutTMVar, tryReadTBQueue,
                     tryReadTChan, tryReadTMVar, tryReadTQueue, tryTakeTMVar,
                     unGetTBQueue, unGetTChan, unGetTQueue, writeTBQueue,
                     writeTChan, writeTQueue, writeTVar)
import qualified Control.Monad.Class.MonadSTM as Lazy
import           Data.Array.Base (MArray (..))
import           GHC.Stack
import           Numeric.Natural (Natural)

{-------------------------------------------------------------------------------
  Lazy TVar
-------------------------------------------------------------------------------}

type LazyTVar    m = Lazy.TVar m
type LazyTMVar   m = Lazy.TMVar m
type LazyTQueue  m = Lazy.TQueue m
type LazyTBQueue m = Lazy.TBQueue m
type LazyTArray  m = Lazy.TArray m
type LazyTChan   m = Lazy.TChan m

{-------------------------------------------------------------------------------
  Strict TVar
-------------------------------------------------------------------------------}

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
            => proxy m
            -> StrictTVar m a
            -> (Maybe a -> a -> InspectMonad m TraceValue)
            -> m ()
traceTVarIO p StrictTVar {tvar} = Lazy.traceTVarIO p tvar

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

newTVarM :: MonadSTM m => a -> m (StrictTVar m a)
newTVarM = newTVarIO
{-# DEPRECATED newTVarM "Use newTVarIO" #-}

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

newTVarWithInvariantM :: (MonadSTM m, HasCallStack)
                      => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                      -> a
                      -> m (StrictTVar m a)
newTVarWithInvariantM = newTVarWithInvariantIO
{-# DEPRECATED newTVarWithInvariantM "Use newTVarWithInvariantIO" #-}

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


updateTVar :: MonadSTM m => StrictTVar m s -> (s -> (a, s)) -> STM m a
updateTVar = stateTVar
{-# DEPRECATED updateTVar "Use stateTVar" #-}

{-------------------------------------------------------------------------------
  Strict TMVar
-------------------------------------------------------------------------------}

-- 'TMVar' that keeps its value in WHNF at all times
--
-- Does not support an invariant: if the invariant would not be satisfied,
-- we would not be able to put a value into an empty TMVar, which would lead
-- to very hard to debug bugs where code is blocked indefinitely.
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

traceTMVarIO :: MonadTraceSTM m
             => proxy m
             -> StrictTMVar m a
             -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
             -> m ()
traceTMVarIO p (StrictTMVar var) = Lazy.traceTMVarIO p var

castStrictTMVar :: LazyTMVar m ~ LazyTMVar n
                => StrictTMVar m a -> StrictTMVar n a
castStrictTMVar (StrictTMVar var) = StrictTMVar var

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = StrictTMVar <$> Lazy.newTMVar a

newTMVarIO :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarIO !a = StrictTMVar <$> Lazy.newTMVarIO a

newTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarM = newTMVarIO
{-# DEPRECATED newTMVarM "Use newTVarIO" #-}

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = StrictTMVar <$> Lazy.newEmptyTMVar

newEmptyTMVarIO :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarIO = StrictTMVar <$> Lazy.newEmptyTMVarIO

newEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarM = newEmptyTMVarIO
{-# DEPRECATED newEmptyTMVarM "Use newEmptyTMVarIO" #-}

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

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar (StrictTMVar tmvar) = Lazy.isEmptyTMVar tmvar

{-------------------------------------------------------------------------------
  Strict TQueue
-------------------------------------------------------------------------------}

newtype StrictTQueue m a = StrictTQueue { toLazyTQueue :: LazyTQueue m a }

fromLazyTQueue :: LazyTQueue m a -> StrictTQueue m a
fromLazyTQueue = StrictTQueue

labelTQueue :: MonadLabelledSTM m => StrictTQueue m a -> String -> STM m ()
labelTQueue (StrictTQueue queue) = Lazy.labelTQueue queue

labelTQueueIO :: MonadLabelledSTM m => StrictTQueue m a -> String -> m ()
labelTQueueIO (StrictTQueue queue) = Lazy.labelTQueueIO queue

traceTQueue :: MonadTraceSTM m
            => proxy m
            -> StrictTQueue m a
            -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
            -> STM m ()
traceTQueue p (StrictTQueue queue) = Lazy.traceTQueue p queue

traceTQueueIO :: MonadTraceSTM m
              => proxy m
              -> StrictTQueue m a
              -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
              -> m ()
traceTQueueIO p (StrictTQueue queue) = Lazy.traceTQueueIO p queue

newTQueue :: MonadSTM m => STM m (StrictTQueue m a)
newTQueue = StrictTQueue <$> Lazy.newTQueue

newTQueueIO :: MonadSTM m => m (StrictTQueue m a)
newTQueueIO = atomically newTQueue

readTQueue :: MonadSTM m => StrictTQueue m a -> STM m a
readTQueue = Lazy.readTQueue . toLazyTQueue

tryReadTQueue :: MonadSTM m => StrictTQueue m a -> STM m (Maybe a)
tryReadTQueue = Lazy.tryReadTQueue . toLazyTQueue

peekTQueue :: MonadSTM m => StrictTQueue m a -> STM m a
peekTQueue = Lazy.peekTQueue . toLazyTQueue

tryPeekTQueue :: MonadSTM m => StrictTQueue m a -> STM m (Maybe a)
tryPeekTQueue = Lazy.tryPeekTQueue . toLazyTQueue

writeTQueue :: MonadSTM m => StrictTQueue m a -> a -> STM m ()
writeTQueue (StrictTQueue tqueue) !a = Lazy.writeTQueue tqueue a

isEmptyTQueue  :: MonadSTM m => StrictTQueue m a -> STM m Bool
isEmptyTQueue = Lazy.isEmptyTQueue . toLazyTQueue

unGetTQueue :: MonadSTM m => StrictTQueue m a -> a -> STM m ()
unGetTQueue (StrictTQueue queue) !a = Lazy.unGetTQueue queue a

{-------------------------------------------------------------------------------
  Strict TBQueue
-------------------------------------------------------------------------------}

newtype StrictTBQueue m a = StrictTBQueue { toLazyTBQueue :: LazyTBQueue m a }

fromLazyTBQueue :: LazyTBQueue m a -> StrictTBQueue m a
fromLazyTBQueue = StrictTBQueue

labelTBQueue :: MonadLabelledSTM m => StrictTBQueue m a -> String -> STM m ()
labelTBQueue (StrictTBQueue queue) = Lazy.labelTBQueue queue

labelTBQueueIO :: MonadLabelledSTM m => StrictTBQueue m a -> String -> m ()
labelTBQueueIO (StrictTBQueue queue) = Lazy.labelTBQueueIO queue

traceTBQueue :: MonadTraceSTM m
             => proxy m
             -> StrictTBQueue m a
             -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
             -> STM m ()
traceTBQueue p (StrictTBQueue queue) = Lazy.traceTBQueue p queue

traceTBQueueIO :: MonadTraceSTM m
               => proxy m
               -> StrictTBQueue m a
               -> ((Maybe [a]) -> [a] -> InspectMonad m TraceValue)
               -> m ()
traceTBQueueIO p (StrictTBQueue queue) = Lazy.traceTBQueueIO p queue

newTBQueue :: MonadSTM m => Natural -> STM m (StrictTBQueue m a)
newTBQueue n = StrictTBQueue <$> Lazy.newTBQueue n

newTBQueueIO :: MonadSTM m => Natural -> m (StrictTBQueue m a)
newTBQueueIO = atomically . newTBQueue

readTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m a
readTBQueue = Lazy.readTBQueue . toLazyTBQueue

tryReadTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m (Maybe a)
tryReadTBQueue = Lazy.tryReadTBQueue . toLazyTBQueue

peekTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m a
peekTBQueue = Lazy.peekTBQueue . toLazyTBQueue

tryPeekTBQueue :: MonadSTM m => StrictTBQueue m a -> STM m (Maybe a)
tryPeekTBQueue = Lazy.tryPeekTBQueue . toLazyTBQueue

writeTBQueue :: MonadSTM m => StrictTBQueue m a -> a -> STM m ()
writeTBQueue (StrictTBQueue tqueue) !a = Lazy.writeTBQueue tqueue a

lengthTBQueue  :: MonadSTM m => StrictTBQueue m a -> STM m Natural
lengthTBQueue = Lazy.lengthTBQueue . toLazyTBQueue

isEmptyTBQueue  :: MonadSTM m => StrictTBQueue m a -> STM m Bool
isEmptyTBQueue = Lazy.isEmptyTBQueue . toLazyTBQueue

isFullTBQueue  :: MonadSTM m => StrictTBQueue m a -> STM m Bool
isFullTBQueue = Lazy.isFullTBQueue . toLazyTBQueue

unGetTBQueue :: MonadSTM m => StrictTBQueue m a -> a -> STM m ()
unGetTBQueue (StrictTBQueue queue) !a = Lazy.unGetTBQueue queue a

{-------------------------------------------------------------------------------
  StrictTArray
-------------------------------------------------------------------------------}

newtype StrictTArray m i e = StrictTArray { toLazyTArray :: LazyTArray m i e }

fromLazyTArray :: LazyTArray m i e -> StrictTArray m i e
fromLazyTArray = StrictTArray

instance ( MArray (Lazy.TArray m) e stm
         , Monad stm
         )
      => MArray (StrictTArray m) e stm where
    getBounds (StrictTArray arr) = getBounds arr
    newArray  b !e = StrictTArray <$> newArray b e
    newArray_ b    = StrictTArray <$> newArray_ b
    unsafeRead     (StrictTArray arr) i    = unsafeRead arr i
    unsafeWrite    (StrictTArray arr) i !e = unsafeWrite arr i e
    getNumElements (StrictTArray arr)      = getNumElements arr

{-------------------------------------------------------------------------------
  StrictTChan
-------------------------------------------------------------------------------}

newtype StrictTChan m a = StrictTChan { toLazyTChan :: LazyTChan m a }

fromLazyTChan :: LazyTChan m a -> StrictTChan m a
fromLazyTChan = StrictTChan

newTChan :: MonadSTM m => STM m (StrictTChan m a)
newTChan = StrictTChan <$> Lazy.newTChan

newBroadcastTChan :: MonadSTM m => STM m (StrictTChan m a)
newBroadcastTChan = StrictTChan <$> Lazy.newBroadcastTChan

writeTChan :: MonadSTM m => StrictTChan m a -> a -> STM m ()
writeTChan (StrictTChan chan) !a = Lazy.writeTChan chan a

readTChan :: MonadSTM m => StrictTChan m a -> STM m a
readTChan = Lazy.readTChan . toLazyTChan

tryReadTChan :: MonadSTM m => StrictTChan m a -> STM m (Maybe a)
tryReadTChan = Lazy.tryReadTChan . toLazyTChan

peekTChan :: MonadSTM m => StrictTChan m a -> STM m a
peekTChan = Lazy.peekTChan . toLazyTChan

tryPeekTChan :: MonadSTM m => StrictTChan m a -> STM m (Maybe a)
tryPeekTChan = Lazy.tryPeekTChan . toLazyTChan

dupTChan :: MonadSTM m => StrictTChan m a -> STM m (StrictTChan m a)
dupTChan = fmap fromLazyTChan . Lazy.dupTChan . toLazyTChan

unGetTChan :: MonadSTM m => StrictTChan m a -> a -> STM m ()
unGetTChan (StrictTChan chan) !a = Lazy.unGetTChan chan a

isEmptyTChan :: MonadSTM m => StrictTChan m a -> STM m Bool
isEmptyTChan = Lazy.isEmptyTChan . toLazyTChan

cloneTChan :: MonadSTM m => StrictTChan m a -> STM m (StrictTChan m a)
cloneTChan = fmap fromLazyTChan . Lazy.cloneTChan . toLazyTChan

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
