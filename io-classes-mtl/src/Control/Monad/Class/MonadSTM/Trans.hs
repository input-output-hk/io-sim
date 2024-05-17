{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- undecidable instances needed for 'ContTSTM' instances of
-- 'MonadThrow' and 'MonadCatch' type classes.
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Control.Monad.Class.MonadSTM.Trans
  ( ContTSTM (..)) where

import           Control.Monad.Cont (ContT (..))
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict

import qualified Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadSTM.Internal

import           Data.Array.Base (MArray (..))
import           Data.Function (on)
import           Data.Kind (Type)


-- | A newtype wrapper for an 'STM' monad for 'ContT'
--
newtype ContTSTM r (m :: Type -> Type) a = ContTSTM { getContTSTM :: STM m a }

deriving instance MonadSTM m => Functor     (ContTSTM r m)
deriving instance MonadSTM m => Applicative (ContTSTM r m)
deriving instance MonadSTM m => Monad       (ContTSTM r m)

instance ( Semigroup a, MonadSTM m ) => Semigroup (ContTSTM r m a) where
    a <> b = (<>) <$> a <*> b
instance ( Monoid a, MonadSTM m )    => Monoid (ContTSTM r m a) where
    mempty = pure mempty

instance ( MonadSTM m, MArray e a (STM m) ) => MArray e a (ContTSTM r m) where
    getBounds         = ContTSTM . getBounds
    getNumElements    = ContTSTM . getNumElements
    unsafeRead arr    = ContTSTM . unsafeRead arr
    unsafeWrite arr i = ContTSTM . unsafeWrite arr i
#if __GLASGOW_HASKELL__ >= 910
    newArray idxs     = ContTSTM . newArray idxs
#endif


-- note: this (and the following) instance requires 'UndecidableInstances'
-- extension because it violates 3rd Paterson condition, however `STM m` will
-- resolve to a concrete type of kind (Type -> Type), and thus no larger than
-- `m` itself, e.g. for `m ~ ReaderT r f`, `STM m ~ WrappedSTM Reader r f`.
-- Instance resolution will terminate as soon as the monad transformer stack
-- depth is exhausted.
instance ( MonadSTM m
         , MonadThrow.MonadThrow (STM m)
         , MonadThrow.MonadCatch (STM m)
         ) => MonadThrow.MonadThrow (ContTSTM r m) where
  throwIO = ContTSTM . MonadThrow.throwIO

instance ( MonadSTM m
         , MonadThrow.MonadThrow (STM m)
         , MonadThrow.MonadCatch (STM m)
         ) => MonadThrow.MonadCatch (ContTSTM r m) where
  catch action handler = ContTSTM
                       $ MonadThrow.catch (getContTSTM action) (getContTSTM . handler)
  generalBracket acquire release use = ContTSTM $
    MonadThrow.generalBracket (getContTSTM    acquire)
                              (getContTSTM .: release)
                              (getContTSTM .  use)

-- | @'ContT' r m@ monad is using underlying @'STM' m@ monad as its stm monad,
-- without transforming it.
--
instance MonadSTM m => MonadSTM (ContT r m) where
    type STM (ContT r m) = ContTSTM r m
    atomically = lift . atomically . getContTSTM

    type TVar (ContT r m) = TVar m
    newTVar        = ContTSTM .  newTVar
    readTVar       = ContTSTM .  readTVar
    writeTVar      = ContTSTM .: writeTVar
    retry          = ContTSTM    retry
    orElse         = ContTSTM .: on orElse getContTSTM

    modifyTVar     = ContTSTM .: modifyTVar
    modifyTVar'    = ContTSTM .: modifyTVar'
    stateTVar      = ContTSTM .: stateTVar
    swapTVar       = ContTSTM .: swapTVar
    check          = ContTSTM .  check

    type TMVar (ContT r m) = TMVar m
    newTMVar       = ContTSTM .  newTMVar
    newEmptyTMVar  = ContTSTM    newEmptyTMVar
    takeTMVar      = ContTSTM .  takeTMVar
    tryTakeTMVar   = ContTSTM .  tryTakeTMVar
    putTMVar       = ContTSTM .: putTMVar
    tryPutTMVar    = ContTSTM .: tryPutTMVar
    readTMVar      = ContTSTM .  readTMVar
    tryReadTMVar   = ContTSTM .  tryReadTMVar
    swapTMVar      = ContTSTM .: swapTMVar
    writeTMVar     = ContTSTM .: writeTMVar
    isEmptyTMVar   = ContTSTM .  isEmptyTMVar

    type TQueue (ContT r m) = TQueue m
    newTQueue      = ContTSTM newTQueue
    readTQueue     = ContTSTM . readTQueue
    tryReadTQueue  = ContTSTM . tryReadTQueue
    peekTQueue     = ContTSTM . peekTQueue
    tryPeekTQueue  = ContTSTM . tryPeekTQueue
    flushTQueue    = ContTSTM .  flushTQueue
    writeTQueue v  = ContTSTM . writeTQueue v
    isEmptyTQueue  = ContTSTM . isEmptyTQueue
    unGetTQueue    = ContTSTM .: unGetTQueue

    type TBQueue (ContT r m) = TBQueue m
    newTBQueue     = ContTSTM .  newTBQueue
    readTBQueue    = ContTSTM .  readTBQueue
    tryReadTBQueue = ContTSTM .  tryReadTBQueue
    peekTBQueue    = ContTSTM .  peekTBQueue
    tryPeekTBQueue = ContTSTM .  tryPeekTBQueue
    flushTBQueue   = ContTSTM .  flushTBQueue
    writeTBQueue   = ContTSTM .: writeTBQueue
    lengthTBQueue  = ContTSTM .  lengthTBQueue
    isEmptyTBQueue = ContTSTM .  isEmptyTBQueue
    isFullTBQueue  = ContTSTM .  isFullTBQueue
    unGetTBQueue   = ContTSTM .: unGetTBQueue

    type TArray (ContT r m) = TArray m

    type TSem (ContT r m) = TSem m
    newTSem        = ContTSTM .  newTSem
    waitTSem       = ContTSTM .  waitTSem
    signalTSem     = ContTSTM .  signalTSem
    signalTSemN    = ContTSTM .: signalTSemN

    type TChan (ContT r m) = TChan m
    newTChan          = ContTSTM    newTChan
    newBroadcastTChan = ContTSTM    newBroadcastTChan
    dupTChan          = ContTSTM .  dupTChan
    cloneTChan        = ContTSTM .  cloneTChan
    readTChan         = ContTSTM .  readTChan
    tryReadTChan      = ContTSTM .  tryReadTChan
    peekTChan         = ContTSTM .  peekTChan
    tryPeekTChan      = ContTSTM .  tryPeekTChan
    writeTChan        = ContTSTM .: writeTChan
    unGetTChan        = ContTSTM .: unGetTChan
    isEmptyTChan      = ContTSTM .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance (Monoid w, MonadSTM m) => MonadSTM (Lazy.WriterT w m) where
    type STM (Lazy.WriterT w m) = Lazy.WriterT w (STM m)
    atomically (Lazy.WriterT stm) = Lazy.WriterT (atomically stm)

    type TVar (Lazy.WriterT w m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (Lazy.WriterT a) (Lazy.WriterT b) = Lazy.WriterT $ a `orElse` b

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (Lazy.WriterT w m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (Lazy.WriterT w m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift .  readTQueue
    tryReadTQueue  = lift .  tryReadTQueue
    peekTQueue     = lift .  peekTQueue
    tryPeekTQueue  = lift .  tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift .  writeTQueue v
    isEmptyTQueue  = lift .  isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (Lazy.WriterT w m) = TBQueue m
    newTBQueue     = lift .  newTBQueue
    readTBQueue    = lift .  readTBQueue
    tryReadTBQueue = lift .  tryReadTBQueue
    peekTBQueue    = lift .  peekTBQueue
    tryPeekTBQueue = lift .  tryPeekTBQueue
    flushTBQueue   = lift .  flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift .  lengthTBQueue
    isEmptyTBQueue = lift .  isEmptyTBQueue
    isFullTBQueue  = lift .  isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (Lazy.WriterT w m) = TArray m

    type TSem (Lazy.WriterT w m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (Lazy.WriterT w m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance (Monoid w, MonadSTM m) => MonadSTM (Strict.WriterT w m) where
    type STM (Strict.WriterT w m) = Strict.WriterT w (STM m)
    atomically (Strict.WriterT stm) = Strict.WriterT (atomically stm)

    type TVar (Strict.WriterT w m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (Strict.WriterT a) (Strict.WriterT b) = Strict.WriterT $ a `orElse` b

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (Strict.WriterT w m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (Strict.WriterT w m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift .  readTQueue
    tryReadTQueue  = lift .  tryReadTQueue
    peekTQueue     = lift .  peekTQueue
    tryPeekTQueue  = lift .  tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift .  writeTQueue v
    isEmptyTQueue  = lift .  isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (Strict.WriterT w m) = TBQueue m
    newTBQueue     = lift .  newTBQueue
    readTBQueue    = lift .  readTBQueue
    tryReadTBQueue = lift .  tryReadTBQueue
    peekTBQueue    = lift .  peekTBQueue
    tryPeekTBQueue = lift .  tryPeekTBQueue
    flushTBQueue   = lift .  flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift .  lengthTBQueue
    isEmptyTBQueue = lift .  isEmptyTBQueue
    isFullTBQueue  = lift .  isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (Strict.WriterT w m) = TArray m

    type TSem (Strict.WriterT w m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (Strict.WriterT w m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance MonadSTM m => MonadSTM (Lazy.StateT s m) where
    type STM (Lazy.StateT s m) = Lazy.StateT s (STM m)
    atomically (Lazy.StateT stm) = Lazy.StateT $ \s -> atomically (stm s)

    type TVar (Lazy.StateT s m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (Lazy.StateT a) (Lazy.StateT b) = Lazy.StateT $ \s -> a s `orElse` b s

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (Lazy.StateT s m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (Lazy.StateT s m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift . readTQueue
    tryReadTQueue  = lift . tryReadTQueue
    peekTQueue     = lift . peekTQueue
    tryPeekTQueue  = lift . tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift . writeTQueue v
    isEmptyTQueue  = lift . isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (Lazy.StateT s m) = TBQueue m
    newTBQueue     = lift .  newTBQueue
    readTBQueue    = lift .  readTBQueue
    tryReadTBQueue = lift .  tryReadTBQueue
    peekTBQueue    = lift .  peekTBQueue
    tryPeekTBQueue = lift .  tryPeekTBQueue
    flushTBQueue   = lift .  flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift .  lengthTBQueue
    isEmptyTBQueue = lift .  isEmptyTBQueue
    isFullTBQueue  = lift .  isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (Lazy.StateT s m) = TArray m

    type TSem (Lazy.StateT s m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (Lazy.StateT s m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance MonadSTM m => MonadSTM (Strict.StateT s m) where
    type STM (Strict.StateT s m) = Strict.StateT s (STM m)
    atomically (Strict.StateT stm) = Strict.StateT $ \s -> atomically (stm s)

    type TVar (Strict.StateT s m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (Strict.StateT a) (Strict.StateT b) = Strict.StateT $ \s -> a s `orElse` b s

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (Strict.StateT s m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (Strict.StateT s m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift . readTQueue
    tryReadTQueue  = lift . tryReadTQueue
    peekTQueue     = lift . peekTQueue
    tryPeekTQueue  = lift . tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift . writeTQueue v
    isEmptyTQueue  = lift . isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (Strict.StateT s m) = TBQueue m
    newTBQueue     = lift .  newTBQueue
    readTBQueue    = lift .  readTBQueue
    tryReadTBQueue = lift .  tryReadTBQueue
    peekTBQueue    = lift .  peekTBQueue
    tryPeekTBQueue = lift .  tryPeekTBQueue
    flushTBQueue   = lift .  flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift .  lengthTBQueue
    isEmptyTBQueue = lift .  isEmptyTBQueue
    isFullTBQueue  = lift .  isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (Strict.StateT s m) = TArray m

    type TSem (Strict.StateT s m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (Strict.StateT s m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance MonadSTM m => MonadSTM (ExceptT e m) where
    type STM (ExceptT e m) = ExceptT e (STM m)
    atomically = ExceptT . atomically . runExceptT

    type TVar (ExceptT e m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse         = ExceptT .: on orElse runExceptT

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (ExceptT e m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (ExceptT e m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift .  readTQueue
    tryReadTQueue  = lift .  tryReadTQueue
    peekTQueue     = lift .  peekTQueue
    tryPeekTQueue  = lift .  tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift .  writeTQueue v
    isEmptyTQueue  = lift .  isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (ExceptT e m) = TBQueue m
    newTBQueue     = lift .  newTBQueue
    readTBQueue    = lift .  readTBQueue
    tryReadTBQueue = lift .  tryReadTBQueue
    peekTBQueue    = lift .  peekTBQueue
    tryPeekTBQueue = lift .  tryPeekTBQueue
    flushTBQueue   = lift .  flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift .  lengthTBQueue
    isEmptyTBQueue = lift .  isEmptyTBQueue
    isFullTBQueue  = lift .  isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (ExceptT e m) = TArray m

    type TSem (ExceptT e m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (ExceptT e m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance (Monoid w, MonadSTM m) => MonadSTM (Lazy.RWST r w s m) where
    type STM (Lazy.RWST r w s m) = Lazy.RWST r w s (STM m)
    atomically (Lazy.RWST stm) = Lazy.RWST $ \r s -> atomically (stm r s)

    type TVar (Lazy.RWST r w s m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (Lazy.RWST a) (Lazy.RWST b) = Lazy.RWST $ \r s -> a r s `orElse` b r s

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (Lazy.RWST r w s m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (Lazy.RWST r w s m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift .  readTQueue
    tryReadTQueue  = lift .  tryReadTQueue
    peekTQueue     = lift .  peekTQueue
    tryPeekTQueue  = lift .  tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift .  writeTQueue v
    isEmptyTQueue  = lift .  isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (Lazy.RWST r w s m) = TBQueue m
    newTBQueue     = lift . newTBQueue
    readTBQueue    = lift . readTBQueue
    tryReadTBQueue = lift . tryReadTBQueue
    peekTBQueue    = lift . peekTBQueue
    tryPeekTBQueue = lift . tryPeekTBQueue
    flushTBQueue   = lift . flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift . lengthTBQueue
    isEmptyTBQueue = lift . isEmptyTBQueue
    isFullTBQueue  = lift . isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (Lazy.RWST r w s m) = TArray m

    type TSem (Lazy.RWST r w s m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (Lazy.RWST r w s m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


-- | The underlying stm monad is also transformed.
--
instance (Monoid w, MonadSTM m) => MonadSTM (Strict.RWST r w s m) where
    type STM (Strict.RWST r w s m) = Strict.RWST r w s (STM m)
    atomically (Strict.RWST stm) = Strict.RWST $ \r s -> atomically (stm r s)

    type TVar (Strict.RWST r w s m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (Strict.RWST a) (Strict.RWST b) = Strict.RWST $ \r s -> a r s `orElse` b r s

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift .  check

    type TMVar (Strict.RWST r w s m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    writeTMVar     = lift .: writeTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (Strict.RWST r w s m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift .  readTQueue
    tryReadTQueue  = lift .  tryReadTQueue
    peekTQueue     = lift .  peekTQueue
    tryPeekTQueue  = lift .  tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift .  writeTQueue v
    isEmptyTQueue  = lift .  isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (Strict.RWST r w s m) = TBQueue m
    newTBQueue     = lift . newTBQueue
    readTBQueue    = lift . readTBQueue
    tryReadTBQueue = lift . tryReadTBQueue
    peekTBQueue    = lift . peekTBQueue
    tryPeekTBQueue = lift . tryPeekTBQueue
    flushTBQueue   = lift . flushTBQueue
    writeTBQueue   = lift .: writeTBQueue
    lengthTBQueue  = lift . lengthTBQueue
    isEmptyTBQueue = lift . isEmptyTBQueue
    isFullTBQueue  = lift . isFullTBQueue
    unGetTBQueue   = lift .: unGetTBQueue

    type TArray (Strict.RWST r w s m) = TArray m

    type TSem (Strict.RWST r w s m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (Strict.RWST r w s m) = TChan m
    newTChan          = lift    newTChan
    newBroadcastTChan = lift    newBroadcastTChan
    dupTChan          = lift .  dupTChan
    cloneTChan        = lift .  cloneTChan
    readTChan         = lift .  readTChan
    tryReadTChan      = lift .  tryReadTChan
    peekTChan         = lift .  peekTChan
    tryPeekTChan      = lift .  tryPeekTChan
    writeTChan        = lift .: writeTChan
    unGetTChan        = lift .: unGetTChan
    isEmptyTChan      = lift .  isEmptyTChan


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
