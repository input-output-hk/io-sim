{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
-- undecidable instances needed for 'WrappedSTM' instances of 'MonadThrow' and
-- 'MonadCatch' type classes.
{-# LANGUAGE UndecidableInstances       #-}

-- Internal module.  It's only exposed as it provides various default types for
-- defining new instances, otherwise prefer to use
-- 'Control.Concurrent.Class.MonadSTM'.
--
module Control.Monad.Class.MonadSTM.Internal
  ( MonadSTM (..)
  , MonadLabelledSTM (..)
  , MonadInspectSTM (..)
  , TraceValue (TraceValue, TraceDynamic, TraceString, DontTrace, traceDynamic, traceString)
  , MonadTraceSTM (..)
  , LazyTVar
  , LazyTMVar
    -- * Default 'TMVar' implementation
  , TMVarDefault (..)
    -- * Default 'TBQueue' implementation
  , TQueueDefault (..)
    -- * Default 'TBQueue' implementation
  , TBQueueDefault (..)
    -- * Default 'TArray' implementation
  , TArrayDefault (..)
    -- * Default 'TSem' implementation
  , TSemDefault (..)
    -- * Default 'TChan' implementation
  , TChanDefault (..)
    -- * MonadThrow aliases
  , throwSTM
  , catchSTM
    -- * Deprecated API
  , newTVarM
  , newTMVarM
  , newTMVarMDefault
  , newEmptyTMVarM
  , newEmptyTMVarMDefault
    -- * Utils
  , WrappedSTM (..)
  ) where

import           Prelude hiding (read)

import qualified Control.Concurrent.STM.TArray as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TSem as STM
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (MonadPlus (..), unless, when)
import qualified Control.Monad.STM as STM

import           Control.Monad.Cont (ContT (..))
import           Control.Monad.Except (ExceptT (..))
import           Control.Monad.RWS (RWST (..))
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.State (StateT (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer (WriterT (..))

import qualified Control.Monad.Class.MonadThrow as MonadThrow

import           Control.Applicative (Alternative (..))
import           Control.Exception
import           Data.Array (Array, bounds)
import qualified Data.Array as Array
import           Data.Array.Base (IArray (numElements), MArray (..),
                     arrEleBottom, listArray, unsafeAt)
import           Data.Foldable (traverse_)
import           Data.Function (on)
import           Data.Ix (Ix, rangeSize)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           GHC.Stack
import           Numeric.Natural (Natural)


{-# DEPRECATED LazyTVar  "Renamed back to 'TVar'" #-}
{-# DEPRECATED LazyTMVar "Renamed back to 'TMVar'" #-}
type LazyTVar  m = TVar m
type LazyTMVar m = TMVar m

-- The STM primitives
class ( Monad m
      , Alternative (STM m)
      , MonadPlus   (STM m)
      ) => MonadSTM m where
  -- STM transactions
  type STM  m = (stm :: Type -> Type)  | stm -> m
  atomically :: HasCallStack => STM m a -> m a

  type TVar m  :: Type -> Type

  newTVar      :: a -> STM m (TVar m a)
  readTVar     :: TVar m a -> STM m a
  writeTVar    :: TVar m a -> a -> STM m ()
  retry        :: STM m a
  orElse       :: STM m a -> STM m a -> STM m a

  modifyTVar   :: TVar m a -> (a -> a) -> STM m ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f

  modifyTVar'  :: TVar m a -> (a -> a) -> STM m ()
  modifyTVar' v f = readTVar v >>= \x -> writeTVar v $! f x

  -- | @since io-classes-0.2.0.0
  stateTVar    :: TVar m s -> (s -> (a, s)) -> STM m a
  stateTVar    = stateTVarDefault

  swapTVar     :: TVar m a -> a -> STM m a
  swapTVar     = swapTVarDefault

  check        :: Bool -> STM m ()
  check True = return ()
  check _    = retry

  -- Additional derived STM APIs
  type TMVar m    :: Type -> Type
  newTMVar        :: a -> STM m (TMVar m a)
  newEmptyTMVar   ::      STM m (TMVar m a)
  takeTMVar       :: TMVar m a      -> STM m a
  tryTakeTMVar    :: TMVar m a      -> STM m (Maybe a)
  putTMVar        :: TMVar m a -> a -> STM m ()
  tryPutTMVar     :: TMVar m a -> a -> STM m Bool
  readTMVar       :: TMVar m a      -> STM m a
  tryReadTMVar    :: TMVar m a      -> STM m (Maybe a)
  swapTMVar       :: TMVar m a -> a -> STM m a
  isEmptyTMVar    :: TMVar m a      -> STM m Bool

  type TQueue m  :: Type -> Type
  newTQueue      :: STM m (TQueue m a)
  readTQueue     :: TQueue m a -> STM m a
  tryReadTQueue  :: TQueue m a -> STM m (Maybe a)
  peekTQueue     :: TQueue m a -> STM m a
  tryPeekTQueue  :: TQueue m a -> STM m (Maybe a)
  flushTQueue    :: TQueue m a -> STM m [a]
  writeTQueue    :: TQueue m a -> a -> STM m ()
  isEmptyTQueue  :: TQueue m a -> STM m Bool
  unGetTQueue    :: TQueue m a -> a -> STM m ()

  type TBQueue m ::  Type -> Type
  newTBQueue     :: Natural -> STM m (TBQueue m a)
  readTBQueue    :: TBQueue m a -> STM m a
  tryReadTBQueue :: TBQueue m a -> STM m (Maybe a)
  peekTBQueue    :: TBQueue m a -> STM m a
  tryPeekTBQueue :: TBQueue m a -> STM m (Maybe a)
  flushTBQueue   :: TBQueue m a -> STM m [a]
  writeTBQueue   :: TBQueue m a -> a -> STM m ()
  -- | @since 0.2.0.0
  lengthTBQueue  :: TBQueue m a -> STM m Natural
  isEmptyTBQueue :: TBQueue m a -> STM m Bool
  isFullTBQueue  :: TBQueue m a -> STM m Bool
  unGetTBQueue   :: TBQueue m a -> a -> STM m ()

  type TArray m  :: Type -> Type -> Type

  type TSem m :: Type
  newTSem     :: Integer -> STM m (TSem m)
  waitTSem    :: TSem m -> STM m ()
  signalTSem  :: TSem m -> STM m ()
  signalTSemN :: Natural -> TSem m -> STM m ()

  type TChan m      :: Type -> Type
  newTChan          :: STM m (TChan m a)
  newBroadcastTChan :: STM m (TChan m a)
  dupTChan          :: TChan m a -> STM m (TChan m a)
  cloneTChan        :: TChan m a -> STM m (TChan m a)
  readTChan         :: TChan m a -> STM m a
  tryReadTChan      :: TChan m a -> STM m (Maybe a)
  peekTChan         :: TChan m a -> STM m a
  tryPeekTChan      :: TChan m a -> STM m (Maybe a)
  writeTChan        :: TChan m a -> a -> STM m ()
  unGetTChan        :: TChan m a -> a -> STM m ()
  isEmptyTChan      :: TChan m a -> STM m Bool


  -- Helpful derived functions with default implementations

  newTVarIO           :: a -> m (TVar  m a)
  readTVarIO          :: TVar m a -> m a
  newTMVarIO          :: a -> m (TMVar m a)
  newEmptyTMVarIO     ::      m (TMVar m a)
  newTQueueIO         :: m (TQueue m a)
  newTBQueueIO        :: Natural -> m (TBQueue m a)
  newTChanIO          :: m (TChan m a)
  newBroadcastTChanIO :: m (TChan m a)

  --
  -- default implementations
  --

  default newTMVar :: TMVar m ~ TMVarDefault m
                   => a -> STM m (TMVar m a)
  newTMVar = newTMVarDefault

  default newEmptyTMVar :: TMVar m ~ TMVarDefault m
                        => STM m (TMVar m a)
  newEmptyTMVar = newEmptyTMVarDefault

  newTVarIO           = atomically . newTVar
  readTVarIO          = atomically . readTVar
  newTMVarIO          = atomically . newTMVar
  newEmptyTMVarIO     = atomically   newEmptyTMVar
  newTQueueIO         = atomically   newTQueue
  newTBQueueIO        = atomically . newTBQueue
  newTChanIO          = atomically   newTChan
  newBroadcastTChanIO = atomically   newBroadcastTChan

  default takeTMVar :: TMVar m ~ TMVarDefault m
                    => TMVar m a -> STM m a
  takeTMVar = takeTMVarDefault

  default tryTakeTMVar :: TMVar m ~ TMVarDefault m
                       => TMVar m a -> STM m (Maybe a)
  tryTakeTMVar = tryTakeTMVarDefault

  default putTMVar :: TMVar m ~ TMVarDefault m => TMVar m a -> a -> STM m ()
  putTMVar = putTMVarDefault

  default tryPutTMVar :: TMVar m ~ TMVarDefault m => TMVar m a -> a -> STM m Bool
  tryPutTMVar = tryPutTMVarDefault

  default readTMVar :: TMVar m ~ TMVarDefault m
                    => TMVar m a -> STM m a
  readTMVar = readTMVarDefault

  default tryReadTMVar :: TMVar m ~ TMVarDefault m
                    => TMVar m a -> STM m (Maybe a)
  tryReadTMVar = tryReadTMVarDefault

  default swapTMVar :: TMVar m ~ TMVarDefault m
                    => TMVar m a -> a -> STM m a
  swapTMVar = swapTMVarDefault

  default isEmptyTMVar :: TMVar m ~ TMVarDefault m
                       => TMVar m a -> STM m Bool
  isEmptyTMVar = isEmptyTMVarDefault

  default newTQueue :: TQueue m ~ TQueueDefault m
                    => STM m (TQueue m a)
  newTQueue = newTQueueDefault

  default writeTQueue :: TQueue m ~ TQueueDefault m
                      => TQueue m a -> a -> STM m ()
  writeTQueue = writeTQueueDefault

  default readTQueue :: TQueue m ~ TQueueDefault m
                     => TQueue m a -> STM m a
  readTQueue = readTQueueDefault

  default tryReadTQueue :: TQueue m ~ TQueueDefault m
                         => TQueue m a -> STM m (Maybe a)
  tryReadTQueue = tryReadTQueueDefault

  default isEmptyTQueue :: TQueue m ~ TQueueDefault m
                        => TQueue m a -> STM m Bool
  isEmptyTQueue = isEmptyTQueueDefault

  default unGetTQueue :: TQueue m ~ TQueueDefault m
                      => TQueue m a -> a -> STM m ()
  unGetTQueue = unGetTQueueDefault

  default peekTQueue :: TQueue m ~ TQueueDefault m
                     => TQueue m a -> STM m a
  peekTQueue = peekTQueueDefault

  default tryPeekTQueue :: TQueue m ~ TQueueDefault m
                        => TQueue m a -> STM m (Maybe a)
  tryPeekTQueue = tryPeekTQueueDefault

  default flushTQueue :: TQueue m ~ TQueueDefault m
                      => TQueue m a -> STM m [a]
  flushTQueue = flushTQueueDefault

  default newTBQueue :: TBQueue m ~ TBQueueDefault m
                     => Natural -> STM m (TBQueue m a)
  newTBQueue = newTBQueueDefault

  default writeTBQueue :: TBQueue m ~ TBQueueDefault m
                       => TBQueue m a -> a -> STM m ()
  writeTBQueue = writeTBQueueDefault

  default readTBQueue :: TBQueue m ~ TBQueueDefault m
                      => TBQueue m a -> STM m a
  readTBQueue = readTBQueueDefault

  default tryReadTBQueue :: TBQueue m ~ TBQueueDefault m
                          => TBQueue m a -> STM m (Maybe a)
  tryReadTBQueue = tryReadTBQueueDefault

  default isEmptyTBQueue :: TBQueue m ~ TBQueueDefault m
                         => TBQueue m a -> STM m Bool
  isEmptyTBQueue = isEmptyTBQueueDefault

  default peekTBQueue :: TBQueue m ~ TBQueueDefault m
                      => TBQueue m a -> STM m a
  peekTBQueue = peekTBQueueDefault

  default tryPeekTBQueue :: TBQueue m ~ TBQueueDefault m
                         => TBQueue m a -> STM m (Maybe a)
  tryPeekTBQueue = tryPeekTBQueueDefault

  default isFullTBQueue :: TBQueue m ~ TBQueueDefault m
                        => TBQueue m a -> STM m Bool
  isFullTBQueue = isFullTBQueueDefault

  default lengthTBQueue :: TBQueue m ~ TBQueueDefault m
                        => TBQueue m a -> STM m Natural
  lengthTBQueue = lengthTBQueueDefault

  default flushTBQueue :: TBQueue m ~ TBQueueDefault m
                       => TBQueue m a -> STM m [a]
  flushTBQueue = flushTBQueueDefault

  default unGetTBQueue :: TBQueue m ~ TBQueueDefault m
                      => TBQueue m a -> a -> STM m ()
  unGetTBQueue = unGetTBQueueDefault

  default newTSem :: TSem m ~ TSemDefault m
                  => Integer -> STM m (TSem m)
  newTSem = newTSemDefault

  default waitTSem :: TSem m ~ TSemDefault m
                   => TSem m -> STM m ()
  waitTSem = waitTSemDefault

  default signalTSem :: TSem m ~ TSemDefault m
                     => TSem m -> STM m ()
  signalTSem = signalTSemDefault

  default signalTSemN :: TSem m ~ TSemDefault m
                      => Natural -> TSem m -> STM m ()
  signalTSemN = signalTSemNDefault

  default newTChan :: TChan m ~ TChanDefault m
                   => STM m (TChan m a)
  newTChan = newTChanDefault

  default newBroadcastTChan :: TChan m ~ TChanDefault m
                            => STM m (TChan m a)
  newBroadcastTChan = newBroadcastTChanDefault

  default writeTChan :: TChan m ~ TChanDefault m
                     => TChan m a -> a -> STM m ()
  writeTChan = writeTChanDefault

  default readTChan :: TChan m ~ TChanDefault m
                     => TChan m a -> STM m a
  readTChan = readTChanDefault

  default tryReadTChan :: TChan m ~ TChanDefault m
                       => TChan m a -> STM m (Maybe a)
  tryReadTChan = tryReadTChanDefault

  default peekTChan :: TChan m ~ TChanDefault m
                     => TChan m a -> STM m a
  peekTChan = peekTChanDefault

  default tryPeekTChan :: TChan m ~ TChanDefault m
                       => TChan m a -> STM m (Maybe a)
  tryPeekTChan = tryPeekTChanDefault

  default dupTChan :: TChan m ~ TChanDefault m
                   => TChan m a -> STM m (TChan m a)
  dupTChan = dupTChanDefault

  default unGetTChan :: TChan m ~ TChanDefault m
                     => TChan m a -> a -> STM m ()
  unGetTChan = unGetTChanDefault

  default isEmptyTChan :: TChan m ~ TChanDefault m
                        => TChan m a -> STM m Bool
  isEmptyTChan = isEmptyTChanDefault

  default cloneTChan :: TChan m ~ TChanDefault m
                     => TChan m a -> STM m (TChan m a)
  cloneTChan = cloneTChanDefault


stateTVarDefault :: MonadSTM m => TVar m s -> (s -> (a, s)) -> STM m a
stateTVarDefault var f = do
   s <- readTVar var
   let (a, s') = f s
   writeTVar var s'
   return a

swapTVarDefault :: MonadSTM m => TVar m a -> a -> STM m a
swapTVarDefault var new = do
    old <- readTVar var
    writeTVar var new
    return old


newTVarM :: MonadSTM m => a -> m (TVar  m a)
newTVarM = newTVarIO
{-# DEPRECATED newTVarM "Use newTVarIO" #-}

newTMVarM :: MonadSTM m => a -> m (TMVar m a)
newTMVarM = newTMVarIO
{-# DEPRECATED newTMVarM "Use newTMVarIO" #-}

newEmptyTMVarM  :: MonadSTM m => m (TMVar m a)
newEmptyTMVarM = newEmptyTMVarIO
{-# DEPRECATED newEmptyTMVarM "Use newEmptyTMVarIO" #-}


-- | Labelled 'TVar's, 'TMVar's, 'TQueue's and 'TBQueue's.
--
class MonadSTM m
   => MonadLabelledSTM m where
  labelTVar    :: TVar    m a   -> String -> STM m ()
  labelTMVar   :: TMVar   m a   -> String -> STM m ()
  labelTQueue  :: TQueue  m a   -> String -> STM m ()
  labelTBQueue :: TBQueue m a   -> String -> STM m ()
  labelTArray  :: (Ix i, Show i)
               => TArray  m i e -> String -> STM m ()
  labelTSem    :: TSem    m     -> String -> STM m ()
  labelTChan   :: TChan   m a   -> String -> STM m ()

  labelTVarIO    :: TVar    m a   -> String -> m ()
  labelTMVarIO   :: TMVar   m a   -> String -> m ()
  labelTQueueIO  :: TQueue  m a   -> String -> m ()
  labelTBQueueIO :: TBQueue m a   -> String -> m ()
  labelTArrayIO  :: (Ix i, Show i)
                 => TArray  m i e -> String -> m ()
  labelTSemIO    :: TSem    m     -> String -> m ()
  labelTChanIO   :: TChan   m a   -> String -> m ()

  --
  -- default implementations
  --

  default labelTMVar :: TMVar m ~ TMVarDefault m
                     => TMVar m a -> String -> STM m ()
  labelTMVar = labelTMVarDefault

  default labelTQueue :: TQueue m ~ TQueueDefault m
                      => TQueue m a -> String -> STM m ()
  labelTQueue = labelTQueueDefault

  default labelTBQueue :: TBQueue m ~ TBQueueDefault m
                       => TBQueue m a -> String -> STM m ()
  labelTBQueue = labelTBQueueDefault

  default labelTSem :: TSem m ~ TSemDefault m
                    => TSem m -> String -> STM m ()
  labelTSem = labelTSemDefault

  default labelTChan :: TChan m ~ TChanDefault m
                     => TChan m a -> String -> STM m ()
  labelTChan = labelTChanDefault

  default labelTArray :: ( TArray m ~ TArrayDefault m
                         , Ix i
                         , Show i
                         )
                      => TArray m i e -> String -> STM m ()
  labelTArray = labelTArrayDefault

  default labelTVarIO :: TVar m a -> String -> m ()
  labelTVarIO = \v l -> atomically (labelTVar v l)

  default labelTMVarIO :: TMVar m a -> String -> m ()
  labelTMVarIO = \v l -> atomically (labelTMVar v l)

  default labelTQueueIO :: TQueue m a -> String -> m ()
  labelTQueueIO = \v l -> atomically (labelTQueue v l)

  default labelTBQueueIO :: TBQueue m a -> String -> m ()
  labelTBQueueIO = \v l -> atomically (labelTBQueue v l)

  default labelTArrayIO :: (Ix i, Show i)
                        => TArray m i e -> String -> m ()
  labelTArrayIO = \v l -> atomically (labelTArray v l)

  default labelTSemIO :: TSem m -> String -> m ()
  labelTSemIO = \v l -> atomically (labelTSem v l)

  default labelTChanIO :: TChan m a -> String -> m ()
  labelTChanIO = \v l -> atomically (labelTChan v l)


-- | This type class is indented for 'io-sim', where one might want to access
-- 'TVar' in the underlying 'ST' monad.
--
class ( MonadSTM m
      , Monad (InspectMonad m)
      )
    => MonadInspectSTM m where
    type InspectMonad m :: Type -> Type
    inspectTVar  :: proxy m -> TVar  m a -> InspectMonad m a
    inspectTMVar :: proxy m -> TMVar m a -> InspectMonad m (Maybe a)
    -- TODO: inspectTQueue, inspectTBQueue

instance MonadInspectSTM IO where
    type InspectMonad IO = IO
    inspectTVar  _ = readTVarIO
    -- issue #3198: tryReadTMVarIO
    inspectTMVar _ = atomically . tryReadTMVar


-- | A GADT which instructs how to trace the value.  The 'traceDynamic' will
-- use dynamic tracing, e.g. 'Control.Monad.IOSim.traceM'; while 'traceString'
-- will be traced with 'EventSay'.
--
data TraceValue where
    TraceValue :: forall tr. Typeable tr
               => { traceDynamic :: Maybe tr
                  , traceString  :: Maybe String
                  }
               -> TraceValue


-- | Use only dynamic tracer.
--
pattern TraceDynamic :: () => forall tr. Typeable tr => tr -> TraceValue
pattern TraceDynamic tr <- TraceValue { traceDynamic = Just tr }
  where
    TraceDynamic tr = TraceValue { traceDynamic = Just tr, traceString = Nothing }

-- | Use only string tracing.
--
pattern TraceString :: String -> TraceValue
pattern TraceString tr <- TraceValue { traceString = Just tr }
  where
    TraceString tr = TraceValue { traceDynamic = (Nothing :: Maybe ())
                                , traceString  = Just tr
                                }

-- | Do not trace the value.
--
pattern DontTrace :: TraceValue
pattern DontTrace <- TraceValue Nothing Nothing
  where
    DontTrace = TraceValue (Nothing :: Maybe ()) Nothing

-- | 'MonadTraceSTM' allows to trace values of stm variables when stm
-- transaction is committed.  This allows to verify invariants when a variable
-- is committed.
--
class MonadInspectSTM m
   => MonadTraceSTM m where
  -- | Construct a trace out of previous & new value of a 'TVar'.  The callback
  -- is called whenever an stm transaction which modifies the 'TVar' is
  -- committed.
  --
  -- This is supported by 'IOSim' and 'IOSimPOR'; 'IO' has a trivial instance.
  --
  {-# MINIMAL traceTVar, traceTQueue, traceTBQueue #-}

  traceTVar    :: proxy m
               -> TVar m a
               -> (Maybe a -> a -> InspectMonad m TraceValue)
               -- ^ callback which receives initial value or 'Nothing' (if it
               -- is a newly created 'TVar'), and the committed value.
               -> STM m ()


  traceTMVar   :: proxy m
               -> TMVar m a
               -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
               -> STM m ()

  traceTQueue  :: proxy m
               -> TQueue m a
               -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
               -> STM m ()

  traceTBQueue :: proxy m
               -> TBQueue m a
               -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
               -> STM m ()

  traceTSem    :: proxy m
               -> TSem m
               -> (Maybe Integer -> Integer -> InspectMonad m TraceValue)
               -> STM m ()

  default traceTMVar :: TMVar m a ~ TMVarDefault m a
                     => proxy m
                     -> TMVar m a
                     -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
                     -> STM m ()
  traceTMVar = traceTMVarDefault

  default traceTSem :: TSem m ~ TSemDefault m
                    => proxy m
                    -> TSem m
                    -> (Maybe Integer -> Integer -> InspectMonad m TraceValue)
                    -> STM m ()
  traceTSem = traceTSemDefault


  traceTVarIO    :: TVar m a
                 -> (Maybe a -> a -> InspectMonad m TraceValue)
                 -> m ()

  traceTMVarIO   :: TMVar m a
                 -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
                 -> m ()

  traceTQueueIO  :: TQueue m a
                 -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
                 -> m ()

  traceTBQueueIO :: TBQueue m a
                 -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
                 -> m ()

  traceTSemIO    :: TSem m
                 -> (Maybe Integer -> Integer -> InspectMonad m TraceValue)
                 -> m ()

  default traceTVarIO :: TVar m a
                      -> (Maybe a -> a -> InspectMonad m TraceValue)
                      -> m ()
  traceTVarIO = \v f -> atomically (traceTVar Proxy v f)

  default traceTMVarIO :: TMVar m a
                       -> (Maybe (Maybe a) -> (Maybe a) -> InspectMonad m TraceValue)
                       -> m ()
  traceTMVarIO = \v f -> atomically (traceTMVar Proxy v f)

  default traceTQueueIO :: TQueue m a
                        -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
                        -> m ()
  traceTQueueIO = \v f -> atomically (traceTQueue Proxy v f)

  default traceTBQueueIO :: TBQueue m a
                         -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
                         -> m ()
  traceTBQueueIO = \v f -> atomically (traceTBQueue Proxy v f)

  default traceTSemIO :: TSem m
                      -> (Maybe Integer -> Integer -> InspectMonad m TraceValue)
                      -> m ()
  traceTSemIO = \v f -> atomically (traceTSem Proxy v f)


--
-- Instance for IO uses the existing STM library implementations
--

instance MonadSTM IO where
  type STM IO = STM.STM

  atomically = wrapBlockedIndefinitely . STM.atomically

  type TVar    IO = STM.TVar
  type TMVar   IO = STM.TMVar
  type TQueue  IO = STM.TQueue
  type TBQueue IO = STM.TBQueue
  type TArray  IO = STM.TArray
  type TSem    IO = STM.TSem
  type TChan   IO = STM.TChan

  newTVar        = STM.newTVar
  readTVar       = STM.readTVar
  writeTVar      = STM.writeTVar
  retry          = STM.retry
  orElse         = STM.orElse
  modifyTVar     = STM.modifyTVar
  modifyTVar'    = STM.modifyTVar'
  stateTVar      = STM.stateTVar
  swapTVar       = STM.swapTVar
  check          = STM.check
  newTMVar       = STM.newTMVar
  newEmptyTMVar  = STM.newEmptyTMVar
  takeTMVar      = STM.takeTMVar
  tryTakeTMVar   = STM.tryTakeTMVar
  putTMVar       = STM.putTMVar
  tryPutTMVar    = STM.tryPutTMVar
  readTMVar      = STM.readTMVar
  tryReadTMVar   = STM.tryReadTMVar
  swapTMVar      = STM.swapTMVar
  isEmptyTMVar   = STM.isEmptyTMVar
  newTQueue      = STM.newTQueue
  readTQueue     = STM.readTQueue
  tryReadTQueue  = STM.tryReadTQueue
  peekTQueue     = STM.peekTQueue
  tryPeekTQueue  = STM.tryPeekTQueue
  flushTQueue    = STM.flushTQueue
  writeTQueue    = STM.writeTQueue
  isEmptyTQueue  = STM.isEmptyTQueue
  unGetTQueue    = STM.unGetTQueue
  newTBQueue     = STM.newTBQueue
  readTBQueue    = STM.readTBQueue
  tryReadTBQueue = STM.tryReadTBQueue
  peekTBQueue    = STM.peekTBQueue
  tryPeekTBQueue = STM.tryPeekTBQueue
  writeTBQueue   = STM.writeTBQueue
  flushTBQueue   = STM.flushTBQueue
  lengthTBQueue  = STM.lengthTBQueue
  isEmptyTBQueue = STM.isEmptyTBQueue
  isFullTBQueue  = STM.isFullTBQueue
  unGetTBQueue   = STM.unGetTBQueue
  newTSem        = STM.newTSem
  waitTSem       = STM.waitTSem
  signalTSem     = STM.signalTSem
  signalTSemN    = STM.signalTSemN

  newTChan          = STM.newTChan
  newBroadcastTChan = STM.newBroadcastTChan
  dupTChan          = STM.dupTChan
  cloneTChan        = STM.cloneTChan
  readTChan         = STM.readTChan
  tryReadTChan      = STM.tryReadTChan
  peekTChan         = STM.peekTChan
  tryPeekTChan      = STM.tryPeekTChan
  writeTChan        = STM.writeTChan
  unGetTChan        = STM.unGetTChan
  isEmptyTChan      = STM.isEmptyTChan

  newTVarIO           = STM.newTVarIO
  readTVarIO          = STM.readTVarIO
  newTMVarIO          = STM.newTMVarIO
  newEmptyTMVarIO     = STM.newEmptyTMVarIO
  newTQueueIO         = STM.newTQueueIO
  newTBQueueIO        = STM.newTBQueueIO
  newTChanIO          = STM.newTChanIO
  newBroadcastTChanIO = STM.newBroadcastTChanIO

-- | noop instance
--
instance MonadLabelledSTM IO where
  labelTVar    = \_  _ -> return ()
  labelTMVar   = \_  _ -> return ()
  labelTQueue  = \_  _ -> return ()
  labelTBQueue = \_  _ -> return ()
  labelTArray  = \_  _ -> return ()
  labelTSem    = \_  _ -> return ()
  labelTChan   = \_  _ -> return ()

  labelTVarIO    = \_  _ -> return ()
  labelTMVarIO   = \_  _ -> return ()
  labelTQueueIO  = \_  _ -> return ()
  labelTBQueueIO = \_  _ -> return ()
  labelTArrayIO  = \_  _ -> return ()
  labelTSemIO    = \_  _ -> return ()
  labelTChanIO   = \_  _ -> return ()

-- | noop instance
--
instance MonadTraceSTM IO where
  traceTVar    = \_ _ _ -> return ()
  traceTMVar   = \_ _ _ -> return ()
  traceTQueue  = \_ _ _ -> return ()
  traceTBQueue = \_ _ _ -> return ()
  traceTSem    = \_ _ _ -> return ()

  traceTVarIO    = \_ _ -> return ()
  traceTMVarIO   = \_ _ -> return ()
  traceTQueueIO  = \_ _ -> return ()
  traceTBQueueIO = \_ _ -> return ()
  traceTSemIO    = \_ _ -> return ()

-- | Wrapper around 'BlockedIndefinitelyOnSTM' that stores a call stack
data BlockedIndefinitely = BlockedIndefinitely {
      blockedIndefinitelyCallStack :: CallStack
    , blockedIndefinitelyException :: BlockedIndefinitelyOnSTM
    }
  deriving Show

instance Exception BlockedIndefinitely where
  displayException (BlockedIndefinitely cs e) = unlines [
        displayException e
      , prettyCallStack cs
      ]

wrapBlockedIndefinitely :: HasCallStack => IO a -> IO a
wrapBlockedIndefinitely = handle (throwIO . BlockedIndefinitely callStack)

--
-- Default TMVar implementation in terms of TVars
--

newtype TMVarDefault m a = TMVar (TVar m (Maybe a))

labelTMVarDefault
  :: MonadLabelledSTM m
  => TMVarDefault m a -> String -> STM m ()
labelTMVarDefault (TMVar tvar) = labelTVar tvar

traceTMVarDefault
  :: MonadTraceSTM m
  => proxy m
  -> TMVarDefault m a
  -> (Maybe (Maybe a) -> Maybe a -> InspectMonad m TraceValue)
  -> STM m ()
traceTMVarDefault p (TMVar t) f = traceTVar p t f

newTMVarDefault :: MonadSTM m => a -> STM m (TMVarDefault m a)
newTMVarDefault a = do
  t <- newTVar (Just a)
  return (TMVar t)

newTMVarIODefault :: MonadSTM m => a -> m (TMVarDefault m a)
newTMVarIODefault a = do
  t <- newTVarM (Just a)
  return (TMVar t)
{-# DEPRECATED newTMVarIODefault "MonadSTM provides a default implementation" #-}

newTMVarMDefault :: MonadSTM m => a -> m (TMVarDefault m a)
newTMVarMDefault = newTMVarIODefault
{-# DEPRECATED newTMVarMDefault "Use newTMVarIODefault" #-}

newEmptyTMVarDefault :: MonadSTM m => STM m (TMVarDefault m a)
newEmptyTMVarDefault = do
  t <- newTVar Nothing
  return (TMVar t)

newEmptyTMVarIODefault :: MonadSTM m => m (TMVarDefault m a)
newEmptyTMVarIODefault = do
  t <- newTVarIO Nothing
  return (TMVar t)
{-# DEPRECATED newEmptyTMVarIODefault "MonadSTM provides a default implementation" #-}

newEmptyTMVarMDefault :: MonadSTM m => m (TMVarDefault m a)
newEmptyTMVarMDefault = newEmptyTMVarIODefault
{-# DEPRECATED newEmptyTMVarMDefault "Use newEmptyTMVarIODefault" #-}

takeTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m a
takeTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do writeTVar t Nothing; return a

tryTakeTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m (Maybe a)
tryTakeTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writeTVar t Nothing; return (Just a)

putTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> STM m ()
putTMVarDefault (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return ()
    Just _  -> retry

tryPutTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> STM m Bool
tryPutTMVarDefault (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return True
    Just _  -> return False

readTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m a
readTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

tryReadTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m (Maybe a)
tryReadTMVarDefault (TMVar t) = readTVar t

swapTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> STM m a
swapTMVarDefault (TMVar t) new = do
  m <- readTVar t
  case m of
    Nothing  -> retry
    Just old -> do writeTVar t (Just new); return old

isEmptyTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m Bool
isEmptyTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False

--
-- Default TQueue implementation in terms of TVars (used by sim)
--

data TQueueDefault m a = TQueue !(TVar m [a])
                                !(TVar m [a])

labelTQueueDefault
  :: MonadLabelledSTM m
  => TQueueDefault m a -> String -> STM m ()
labelTQueueDefault (TQueue read write) label = do
  labelTVar read (label ++ "-read")
  labelTVar write (label ++ "-write")

newTQueueDefault :: MonadSTM m => STM m (TQueueDefault m a)
newTQueueDefault = do
  read  <- newTVar []
  write <- newTVar []
  return (TQueue read write)

writeTQueueDefault :: MonadSTM m => TQueueDefault m a -> a -> STM m ()
writeTQueueDefault (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

readTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
readTQueueDefault queue = maybe retry return =<< tryReadTQueueDefault queue

tryReadTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryReadTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return (Just x)
    [] -> do
      ys <- readTVar write
      case reverse ys of
        []     -> return Nothing
        (z:zs) -> do
          writeTVar write []
          writeTVar read zs
          return (Just z)

isEmptyTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m Bool
isEmptyTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

peekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
peekTQueueDefault (TQueue read _write) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return x
      _     -> retry

tryPeekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryPeekTQueueDefault (TQueue read _write) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return (Just x)
      _     -> return Nothing


flushTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m [a]
flushTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  ys <- readTVar write
  unless (null xs) $ writeTVar read []
  unless (null ys) $ writeTVar write []
  return (xs ++ reverse ys)

unGetTQueueDefault :: MonadSTM m => TQueueDefault m a -> a -> STM m ()
unGetTQueueDefault (TQueue read _write) a = modifyTVar read (a:)



--
-- Default TBQueue implementation in terms of TVars
--

data TBQueueDefault m a = TBQueue
  !(TVar m Natural) -- read capacity
  !(TVar m [a])     -- elements waiting for read
  !(TVar m Natural) -- write capacity
  !(TVar m [a])     -- written elements
  !Natural

labelTBQueueDefault
  :: MonadLabelledSTM m
  => TBQueueDefault m a -> String -> STM m ()
labelTBQueueDefault (TBQueue rsize read wsize write _size) label = do
  labelTVar rsize (label ++ "-rsize")
  labelTVar read (label ++ "-read")
  labelTVar wsize (label ++ "-wsize")
  labelTVar write (label ++ "-write")

newTBQueueDefault :: MonadSTM m => Natural -> STM m (TBQueueDefault m a)
newTBQueueDefault size = do
  rsize <- newTVar 0
  read  <- newTVar []
  wsize <- newTVar size
  write <- newTVar []
  return (TBQueue rsize read wsize write size)

readTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
readTBQueueDefault queue = maybe retry return =<< tryReadTBQueueDefault queue

tryReadTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryReadTBQueueDefault (TBQueue rsize read _wsize write _size) = do
  xs <- readTVar read
  r <- readTVar rsize
  writeTVar rsize $! r + 1
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return (Just x)
    [] -> do
      ys <- readTVar write
      case reverse ys of
        [] -> return Nothing

        -- NB. lazy: we want the transaction to be
        -- short, otherwise it will conflict
        (z:zs)  -> do
          writeTVar write []
          writeTVar read zs
          return (Just z)

peekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
peekTBQueueDefault (TBQueue _rsize read _wsize _write _size) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return x
      _     -> retry

tryPeekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryPeekTBQueueDefault (TBQueue _rsize read _wsize _write _size) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return (Just x)
      _     -> return Nothing

writeTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> STM m ()
writeTBQueueDefault (TBQueue rsize _read wsize write _size) a = do
  w <- readTVar wsize
  if (w > 0)
    then do writeTVar wsize $! w - 1
    else do
          r <- readTVar rsize
          if (r > 0)
            then do writeTVar rsize 0
                    writeTVar wsize $! r - 1
            else retry
  listend <- readTVar write
  writeTVar write (a:listend)

isEmptyTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isEmptyTBQueueDefault (TBQueue _rsize read _wsize write _size) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

isFullTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isFullTBQueueDefault (TBQueue rsize _read wsize _write _size) = do
  w <- readTVar wsize
  if (w > 0)
     then return False
     else do
         r <- readTVar rsize
         if (r > 0)
            then return False
            else return True

lengthTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Natural
lengthTBQueueDefault (TBQueue rsize _read wsize _write size) = do
  r <- readTVar rsize
  w <- readTVar wsize
  return $! size - r - w


flushTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m [a]
flushTBQueueDefault (TBQueue rsize read wsize write size) = do
  xs <- readTVar read
  ys <- readTVar write
  if null xs && null ys
    then return []
    else do
      writeTVar read []
      writeTVar write []
      writeTVar rsize 0
      writeTVar wsize size
      return (xs ++ reverse ys)

unGetTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> STM m ()
unGetTBQueueDefault (TBQueue rsize read wsize _write _size) a = do
  r <- readTVar rsize
  if (r > 0)
     then do writeTVar rsize $! r - 1
     else do
          w <- readTVar wsize
          if (w > 0)
             then writeTVar wsize $! w - 1
             else retry
  xs <- readTVar read
  writeTVar read (a:xs)


--
-- Default `TArray` implementation
--

-- | Default implementation of 'TArray'.
--
data TArrayDefault m i e = TArray (Array i (TVar m e))
  deriving Typeable

deriving instance (Eq (TVar m e), Ix i) => Eq (TArrayDefault m i e)

instance (Monad stm, MonadSTM m, stm ~ STM m)
      => MArray (TArrayDefault m) e stm where
    getBounds (TArray a) = return (bounds a)
    newArray b e = do
      a <- rep (rangeSize b) (newTVar e)
      return $ TArray (listArray b a)
    newArray_ b = do
      a <- rep (rangeSize b) (newTVar arrEleBottom)
      return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writeTVar (unsafeAt a i) e
    getNumElements (TArray a) = return (numElements a)

rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)

labelTArrayDefault :: ( MonadLabelledSTM m
                      , Ix i
                      , Show i
                      )
                   => TArrayDefault m i e -> String -> STM m ()
labelTArrayDefault (TArray arr) name = do
    let as = Array.assocs arr
    traverse_ (\(i, v) -> labelTVar v (name ++ ":" ++ show i)) as


--
-- Default `TSem` implementation
--

newtype TSemDefault m = TSem (TVar m Integer)

labelTSemDefault :: MonadLabelledSTM m => TSemDefault m -> String -> STM m ()
labelTSemDefault (TSem t) = labelTVar t

traceTSemDefault :: MonadTraceSTM m
                 => proxy m
                 -> TSemDefault m
                 -> (Maybe Integer -> Integer -> InspectMonad m TraceValue)
                 -> STM m ()
traceTSemDefault proxy (TSem t) k = traceTVar proxy t k

newTSemDefault :: MonadSTM m => Integer -> STM m (TSemDefault m)
newTSemDefault i = TSem <$> (newTVar $! i)

waitTSemDefault :: MonadSTM m => TSemDefault m -> STM m ()
waitTSemDefault (TSem t) = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)

signalTSemDefault :: MonadSTM m => TSemDefault m -> STM m ()
signalTSemDefault (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+1

signalTSemNDefault :: MonadSTM m => Natural -> TSemDefault m -> STM m ()
signalTSemNDefault 0 _ = return ()
signalTSemNDefault 1 s = signalTSemDefault s
signalTSemNDefault n (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+(toInteger n)

--
-- Default `TChan` implementation
--

type TVarList m a = TVar m (TList m a)
data TList m a = TNil | TCons a (TVarList m a)

data TChanDefault m a = TChan (TVar m (TVarList m a)) (TVar m (TVarList m a))

labelTChanDefault :: MonadLabelledSTM m => TChanDefault m a -> String -> STM m ()
labelTChanDefault (TChan read write) name = do
  labelTVar read  (name ++ ":read")
  labelTVar write (name ++ ":write")

newTChanDefault :: MonadSTM m => STM m (TChanDefault m a)
newTChanDefault = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

newBroadcastTChanDefault :: MonadSTM m => STM m (TChanDefault m a)
newBroadcastTChanDefault = do
    write_hole <- newTVar TNil
    read <- newTVar (error "reading from a TChan created by newBroadcastTChan; use dupTChan first")
    write <- newTVar write_hole
    return (TChan read write)

writeTChanDefault :: MonadSTM m => TChanDefault m a -> a -> STM m ()
writeTChanDefault (TChan _read write) a = do
  listend <- readTVar write -- listend == TVar pointing to TNil
  new_listend <- newTVar TNil
  writeTVar listend (TCons a new_listend)
  writeTVar write new_listend

readTChanDefault :: MonadSTM m => TChanDefault m a -> STM m a
readTChanDefault (TChan read _write) = do
  listhead <- readTVar read
  head_ <- readTVar listhead
  case head_ of
    TNil -> retry
    TCons a tail_ -> do
        writeTVar read tail_
        return a

tryReadTChanDefault :: MonadSTM m => TChanDefault m a -> STM m (Maybe a)
tryReadTChanDefault (TChan read _write) = do
  listhead <- readTVar read
  head_ <- readTVar listhead
  case head_ of
    TNil       -> return Nothing
    TCons a tl -> do
      writeTVar read tl
      return (Just a)

peekTChanDefault :: MonadSTM m => TChanDefault m a -> STM m a
peekTChanDefault (TChan read _write) = do
  listhead <- readTVar read
  head_ <- readTVar listhead
  case head_ of
    TNil      -> retry
    TCons a _ -> return a

tryPeekTChanDefault :: MonadSTM m => TChanDefault m a -> STM m (Maybe a)
tryPeekTChanDefault (TChan read _write) = do
  listhead <- readTVar read
  head_ <- readTVar listhead
  case head_ of
    TNil      -> return Nothing
    TCons a _ -> return (Just a)

dupTChanDefault :: MonadSTM m => TChanDefault m a -> STM m (TChanDefault m a)
dupTChanDefault (TChan _read write) = do
  hole <- readTVar write
  new_read <- newTVar hole
  return (TChan new_read write)

unGetTChanDefault :: MonadSTM m => TChanDefault m a -> a -> STM m ()
unGetTChanDefault (TChan read _write) a = do
   listhead <- readTVar read
   newhead <- newTVar (TCons a listhead)
   writeTVar read newhead

isEmptyTChanDefault :: MonadSTM m => TChanDefault m a -> STM m Bool
isEmptyTChanDefault (TChan read _write) = do
  listhead <- readTVar read
  head_ <- readTVar listhead
  case head_ of
    TNil      -> return True
    TCons _ _ -> return False

cloneTChanDefault :: MonadSTM m => TChanDefault m a -> STM m (TChanDefault m a)
cloneTChanDefault (TChan read write) = do
  readpos <- readTVar read
  new_read <- newTVar readpos
  return (TChan new_read write)


-- | 'throwIO' specialised to @stm@ monad.
--
throwSTM :: (MonadSTM m, MonadThrow.MonadThrow (STM m), Exception e)
         => e -> STM m a
throwSTM = MonadThrow.throwIO


-- | 'catch' specialized for an @stm@ monad.
--
catchSTM :: (MonadSTM m, MonadThrow.MonadCatch (STM m), Exception e)
         => STM m a -> (e -> STM m a) -> STM m a
catchSTM = MonadThrow.catch

--
-- Monad Transformers
--

data Trans where
    Cont   :: Trans
    Reader :: Trans
    Writer :: Trans
    State  :: Trans
    Except :: Trans
    RWS    :: Trans


-- | A newtype wrapper for an 'STM' monad for monad transformers.
--
newtype WrappedSTM (t :: Trans) r (m :: Type -> Type) a = WrappedSTM { runWrappedSTM :: STM m a }

deriving instance MonadSTM m => Functor     (WrappedSTM t r m)
deriving instance MonadSTM m => Applicative (WrappedSTM t r m)
deriving instance MonadSTM m => Monad       (WrappedSTM t r m)
deriving instance MonadSTM m => Alternative (WrappedSTM t r m)
deriving instance MonadSTM m => MonadPlus   (WrappedSTM t r m)

-- note: this (and the following) instance requires 'UndecidableInstances'
-- extension because it violates 3rd Paterson condition, however `STM m` will
-- resolve to a concrete type of kind (Type -> Type), and thus no larger than
-- `m` itself, e.g. for `m ~ ReaderT r f`, `STM m ~ WrappedSTM Reader r f`.
-- Instance resolution will terminate as soon as the monad transformer stack
-- depth is exhausted.
instance ( MonadSTM m
         , MonadThrow.MonadThrow (STM m)
         , MonadThrow.MonadCatch (STM m)
         ) => MonadThrow.MonadThrow (WrappedSTM t r m) where
  throwIO = WrappedSTM . MonadThrow.throwIO

instance ( MonadSTM m
         , MonadThrow.MonadThrow (STM m)
         , MonadThrow.MonadCatch (STM m)
         ) => MonadThrow.MonadCatch (WrappedSTM t r m) where
  catch action handler = WrappedSTM
                       $ MonadThrow.catch (runWrappedSTM action) (runWrappedSTM . handler)
  generalBracket acquire release use = WrappedSTM $
    MonadThrow.generalBracket (runWrappedSTM    acquire)
                              (runWrappedSTM .: release)
                              (runWrappedSTM .  use)

instance MonadSTM m => MonadSTM (ContT r m) where
    type STM (ContT r m) = WrappedSTM Cont r m
    atomically = lift . atomically . runWrappedSTM

    type TVar (ContT r m) = TVar m
    newTVar        = WrappedSTM .  newTVar
    readTVar       = WrappedSTM .  readTVar
    writeTVar      = WrappedSTM .: writeTVar
    retry          = WrappedSTM    retry
    orElse         = WrappedSTM .: on orElse runWrappedSTM

    modifyTVar     = WrappedSTM .: modifyTVar
    modifyTVar'    = WrappedSTM .: modifyTVar'
    stateTVar      = WrappedSTM .: stateTVar
    swapTVar       = WrappedSTM .: swapTVar
    check          = WrappedSTM .  check

    type TMVar (ContT r m) = TMVar m
    newTMVar       = WrappedSTM .  newTMVar
    newEmptyTMVar  = WrappedSTM    newEmptyTMVar
    takeTMVar      = WrappedSTM .  takeTMVar
    tryTakeTMVar   = WrappedSTM .  tryTakeTMVar
    putTMVar       = WrappedSTM .: putTMVar
    tryPutTMVar    = WrappedSTM .: tryPutTMVar
    readTMVar      = WrappedSTM .  readTMVar
    tryReadTMVar   = WrappedSTM .  tryReadTMVar
    swapTMVar      = WrappedSTM .: swapTMVar
    isEmptyTMVar   = WrappedSTM .  isEmptyTMVar

    type TQueue (ContT r m) = TQueue m
    newTQueue      = WrappedSTM newTQueue
    readTQueue     = WrappedSTM . readTQueue
    tryReadTQueue  = WrappedSTM . tryReadTQueue
    peekTQueue     = WrappedSTM . peekTQueue
    tryPeekTQueue  = WrappedSTM . tryPeekTQueue
    flushTQueue    = WrappedSTM .  flushTQueue
    writeTQueue v  = WrappedSTM . writeTQueue v
    isEmptyTQueue  = WrappedSTM . isEmptyTQueue
    unGetTQueue    = WrappedSTM .: unGetTQueue

    type TBQueue (ContT r m) = TBQueue m
    newTBQueue     = WrappedSTM .  newTBQueue
    readTBQueue    = WrappedSTM .  readTBQueue
    tryReadTBQueue = WrappedSTM .  tryReadTBQueue
    peekTBQueue    = WrappedSTM .  peekTBQueue
    tryPeekTBQueue = WrappedSTM .  tryPeekTBQueue
    flushTBQueue   = WrappedSTM .  flushTBQueue
    writeTBQueue   = WrappedSTM .: writeTBQueue
    lengthTBQueue  = WrappedSTM .  lengthTBQueue
    isEmptyTBQueue = WrappedSTM .  isEmptyTBQueue
    isFullTBQueue  = WrappedSTM .  isFullTBQueue
    unGetTBQueue   = WrappedSTM .: unGetTBQueue

    type TArray (ContT r m) = TArray m

    type TSem (ContT r m) = TSem m
    newTSem        = WrappedSTM .  newTSem
    waitTSem       = WrappedSTM .  waitTSem
    signalTSem     = WrappedSTM .  signalTSem
    signalTSemN    = WrappedSTM .: signalTSemN

    type TChan (ContT r m) = TChan m
    newTChan          = WrappedSTM    newTChan
    newBroadcastTChan = WrappedSTM    newBroadcastTChan
    dupTChan          = WrappedSTM .  dupTChan
    cloneTChan        = WrappedSTM .  cloneTChan
    readTChan         = WrappedSTM .  readTChan
    tryReadTChan      = WrappedSTM .  tryReadTChan
    peekTChan         = WrappedSTM .  peekTChan
    tryPeekTChan      = WrappedSTM .  tryPeekTChan
    writeTChan        = WrappedSTM .: writeTChan
    unGetTChan        = WrappedSTM .: unGetTChan
    isEmptyTChan      = WrappedSTM .  isEmptyTChan


instance MonadSTM m => MonadSTM (ReaderT r m) where
    type STM (ReaderT r m) = WrappedSTM Reader r m
    atomically = lift . atomically . runWrappedSTM

    type TVar (ReaderT r m) = TVar m
    newTVar        = WrappedSTM .  newTVar
    readTVar       = WrappedSTM .  readTVar
    writeTVar      = WrappedSTM .: writeTVar
    retry          = WrappedSTM    retry
    orElse         = WrappedSTM .: on orElse runWrappedSTM

    modifyTVar     = WrappedSTM .: modifyTVar
    modifyTVar'    = WrappedSTM .: modifyTVar'
    stateTVar      = WrappedSTM .: stateTVar
    swapTVar       = WrappedSTM .: swapTVar
    check          = WrappedSTM  . check

    type TMVar (ReaderT r m) = TMVar m
    newTMVar       = WrappedSTM .  newTMVar
    newEmptyTMVar  = WrappedSTM    newEmptyTMVar
    takeTMVar      = WrappedSTM .  takeTMVar
    tryTakeTMVar   = WrappedSTM .  tryTakeTMVar
    putTMVar       = WrappedSTM .: putTMVar
    tryPutTMVar    = WrappedSTM .: tryPutTMVar
    readTMVar      = WrappedSTM .  readTMVar
    tryReadTMVar   = WrappedSTM .  tryReadTMVar
    swapTMVar      = WrappedSTM .: swapTMVar
    isEmptyTMVar   = WrappedSTM .  isEmptyTMVar

    type TQueue (ReaderT r m) = TQueue m
    newTQueue      = WrappedSTM newTQueue
    readTQueue     = WrappedSTM .  readTQueue
    tryReadTQueue  = WrappedSTM .  tryReadTQueue
    peekTQueue     = WrappedSTM .  peekTQueue
    tryPeekTQueue  = WrappedSTM .  tryPeekTQueue
    flushTQueue    = WrappedSTM .  flushTQueue
    writeTQueue v  = WrappedSTM .  writeTQueue v
    isEmptyTQueue  = WrappedSTM .  isEmptyTQueue
    unGetTQueue    = WrappedSTM .: unGetTQueue

    type TBQueue (ReaderT r m) = TBQueue m
    newTBQueue     = WrappedSTM .  newTBQueue
    readTBQueue    = WrappedSTM .  readTBQueue
    tryReadTBQueue = WrappedSTM .  tryReadTBQueue
    peekTBQueue    = WrappedSTM .  peekTBQueue
    tryPeekTBQueue = WrappedSTM .  tryPeekTBQueue
    flushTBQueue   = WrappedSTM .  flushTBQueue
    writeTBQueue   = WrappedSTM .: writeTBQueue
    lengthTBQueue  = WrappedSTM .  lengthTBQueue
    isEmptyTBQueue = WrappedSTM .  isEmptyTBQueue
    isFullTBQueue  = WrappedSTM .  isFullTBQueue
    unGetTBQueue   = WrappedSTM .: unGetTBQueue

    type TArray (ReaderT r m) = TArray m

    type TSem (ReaderT r m) = TSem m
    newTSem        = WrappedSTM .  newTSem
    waitTSem       = WrappedSTM .  waitTSem
    signalTSem     = WrappedSTM .  signalTSem
    signalTSemN    = WrappedSTM .: signalTSemN

    type TChan (ReaderT r m) = TChan m
    newTChan          = WrappedSTM    newTChan
    newBroadcastTChan = WrappedSTM    newBroadcastTChan
    dupTChan          = WrappedSTM .  dupTChan
    cloneTChan        = WrappedSTM .  cloneTChan
    readTChan         = WrappedSTM .  readTChan
    tryReadTChan      = WrappedSTM .  tryReadTChan
    peekTChan         = WrappedSTM .  peekTChan
    tryPeekTChan      = WrappedSTM .  tryPeekTChan
    writeTChan        = WrappedSTM .: writeTChan
    unGetTChan        = WrappedSTM .: unGetTChan
    isEmptyTChan      = WrappedSTM .  isEmptyTChan


instance (Monoid w, MonadSTM m) => MonadSTM (WriterT w m) where
    type STM (WriterT w m) = WrappedSTM Writer w m
    atomically = lift . atomically . runWrappedSTM

    type TVar (WriterT w m) = TVar m
    newTVar        = WrappedSTM .  newTVar
    readTVar       = WrappedSTM .  readTVar
    writeTVar      = WrappedSTM .: writeTVar
    retry          = WrappedSTM    retry
    orElse         = WrappedSTM .: on orElse runWrappedSTM

    modifyTVar     = WrappedSTM .: modifyTVar
    modifyTVar'    = WrappedSTM .: modifyTVar'
    stateTVar      = WrappedSTM .: stateTVar
    swapTVar       = WrappedSTM .: swapTVar
    check          = WrappedSTM .  check

    type TMVar (WriterT w m) = TMVar m
    newTMVar       = WrappedSTM .  newTMVar
    newEmptyTMVar  = WrappedSTM    newEmptyTMVar
    takeTMVar      = WrappedSTM .  takeTMVar
    tryTakeTMVar   = WrappedSTM .  tryTakeTMVar
    putTMVar       = WrappedSTM .: putTMVar
    tryPutTMVar    = WrappedSTM .: tryPutTMVar
    readTMVar      = WrappedSTM .  readTMVar
    tryReadTMVar   = WrappedSTM .  tryReadTMVar
    swapTMVar      = WrappedSTM .: swapTMVar
    isEmptyTMVar   = WrappedSTM .  isEmptyTMVar

    type TQueue (WriterT w m) = TQueue m
    newTQueue      = WrappedSTM newTQueue
    readTQueue     = WrappedSTM .  readTQueue
    tryReadTQueue  = WrappedSTM .  tryReadTQueue
    peekTQueue     = WrappedSTM .  peekTQueue
    tryPeekTQueue  = WrappedSTM .  tryPeekTQueue
    flushTQueue    = WrappedSTM .  flushTQueue
    writeTQueue v  = WrappedSTM .  writeTQueue v
    isEmptyTQueue  = WrappedSTM .  isEmptyTQueue
    unGetTQueue    = WrappedSTM .: unGetTQueue

    type TBQueue (WriterT w m) = TBQueue m
    newTBQueue     = WrappedSTM .  newTBQueue
    readTBQueue    = WrappedSTM .  readTBQueue
    tryReadTBQueue = WrappedSTM .  tryReadTBQueue
    peekTBQueue    = WrappedSTM .  peekTBQueue
    tryPeekTBQueue = WrappedSTM .  tryPeekTBQueue
    flushTBQueue   = WrappedSTM .  flushTBQueue
    writeTBQueue   = WrappedSTM .: writeTBQueue
    lengthTBQueue  = WrappedSTM .  lengthTBQueue
    isEmptyTBQueue = WrappedSTM .  isEmptyTBQueue
    isFullTBQueue  = WrappedSTM .  isFullTBQueue
    unGetTBQueue   = WrappedSTM .: unGetTBQueue

    type TArray (WriterT w m) = TArray m

    type TSem (WriterT w m) = TSem m
    newTSem        = WrappedSTM .  newTSem
    waitTSem       = WrappedSTM .  waitTSem
    signalTSem     = WrappedSTM .  signalTSem
    signalTSemN    = WrappedSTM .: signalTSemN

    type TChan (WriterT w m) = TChan m
    newTChan          = WrappedSTM    newTChan
    newBroadcastTChan = WrappedSTM    newBroadcastTChan
    dupTChan          = WrappedSTM .  dupTChan
    cloneTChan        = WrappedSTM .  cloneTChan
    readTChan         = WrappedSTM .  readTChan
    tryReadTChan      = WrappedSTM .  tryReadTChan
    peekTChan         = WrappedSTM .  peekTChan
    tryPeekTChan      = WrappedSTM .  tryPeekTChan
    writeTChan        = WrappedSTM .: writeTChan
    unGetTChan        = WrappedSTM .: unGetTChan
    isEmptyTChan      = WrappedSTM .  isEmptyTChan


instance MonadSTM m => MonadSTM (StateT s m) where
    type STM (StateT s m) = WrappedSTM State s m
    atomically = lift . atomically . runWrappedSTM

    type TVar (StateT s m) = TVar m
    newTVar        = WrappedSTM .  newTVar
    readTVar       = WrappedSTM .  readTVar
    writeTVar      = WrappedSTM .: writeTVar
    retry          = WrappedSTM    retry
    orElse         = WrappedSTM .: on orElse runWrappedSTM

    modifyTVar     = WrappedSTM .: modifyTVar
    modifyTVar'    = WrappedSTM .: modifyTVar'
    stateTVar      = WrappedSTM .: stateTVar
    swapTVar       = WrappedSTM .: swapTVar
    check          = WrappedSTM .  check

    type TMVar (StateT s m) = TMVar m
    newTMVar       = WrappedSTM .  newTMVar
    newEmptyTMVar  = WrappedSTM    newEmptyTMVar
    takeTMVar      = WrappedSTM .  takeTMVar
    tryTakeTMVar   = WrappedSTM .  tryTakeTMVar
    putTMVar       = WrappedSTM .: putTMVar
    tryPutTMVar    = WrappedSTM .: tryPutTMVar
    readTMVar      = WrappedSTM .  readTMVar
    tryReadTMVar   = WrappedSTM .  tryReadTMVar
    swapTMVar      = WrappedSTM .: swapTMVar
    isEmptyTMVar   = WrappedSTM .  isEmptyTMVar

    type TQueue (StateT s m) = TQueue m
    newTQueue      = WrappedSTM newTQueue
    readTQueue     = WrappedSTM . readTQueue
    tryReadTQueue  = WrappedSTM . tryReadTQueue
    peekTQueue     = WrappedSTM . peekTQueue
    tryPeekTQueue  = WrappedSTM . tryPeekTQueue
    flushTQueue    = WrappedSTM .  flushTQueue
    writeTQueue v  = WrappedSTM . writeTQueue v
    isEmptyTQueue  = WrappedSTM . isEmptyTQueue
    unGetTQueue    = WrappedSTM .: unGetTQueue

    type TBQueue (StateT s m) = TBQueue m
    newTBQueue     = WrappedSTM .  newTBQueue
    readTBQueue    = WrappedSTM .  readTBQueue
    tryReadTBQueue = WrappedSTM .  tryReadTBQueue
    peekTBQueue    = WrappedSTM .  peekTBQueue
    tryPeekTBQueue = WrappedSTM .  tryPeekTBQueue
    flushTBQueue   = WrappedSTM .  flushTBQueue
    writeTBQueue   = WrappedSTM .: writeTBQueue
    lengthTBQueue  = WrappedSTM .  lengthTBQueue
    isEmptyTBQueue = WrappedSTM .  isEmptyTBQueue
    isFullTBQueue  = WrappedSTM .  isFullTBQueue
    unGetTBQueue   = WrappedSTM .: unGetTBQueue

    type TArray (StateT s m) = TArray m

    type TSem (StateT s m) = TSem m
    newTSem        = WrappedSTM .  newTSem
    waitTSem       = WrappedSTM .  waitTSem
    signalTSem     = WrappedSTM .  signalTSem
    signalTSemN    = WrappedSTM .: signalTSemN

    type TChan (StateT s m) = TChan m
    newTChan          = WrappedSTM    newTChan
    newBroadcastTChan = WrappedSTM    newBroadcastTChan
    dupTChan          = WrappedSTM .  dupTChan
    cloneTChan        = WrappedSTM .  cloneTChan
    readTChan         = WrappedSTM .  readTChan
    tryReadTChan      = WrappedSTM .  tryReadTChan
    peekTChan         = WrappedSTM .  peekTChan
    tryPeekTChan      = WrappedSTM .  tryPeekTChan
    writeTChan        = WrappedSTM .: writeTChan
    unGetTChan        = WrappedSTM .: unGetTChan
    isEmptyTChan      = WrappedSTM .  isEmptyTChan


instance MonadSTM m => MonadSTM (ExceptT e m) where
    type STM (ExceptT e m) = WrappedSTM Except e m
    atomically = lift . atomically . runWrappedSTM

    type TVar (ExceptT e m) = TVar m
    newTVar        = WrappedSTM .  newTVar
    readTVar       = WrappedSTM .  readTVar
    writeTVar      = WrappedSTM .: writeTVar
    retry          = WrappedSTM    retry
    orElse         = WrappedSTM .: on orElse runWrappedSTM

    modifyTVar     = WrappedSTM .: modifyTVar
    modifyTVar'    = WrappedSTM .: modifyTVar'
    stateTVar      = WrappedSTM .: stateTVar
    swapTVar       = WrappedSTM .: swapTVar
    check          = WrappedSTM .  check

    type TMVar (ExceptT e m) = TMVar m
    newTMVar       = WrappedSTM .  newTMVar
    newEmptyTMVar  = WrappedSTM    newEmptyTMVar
    takeTMVar      = WrappedSTM .  takeTMVar
    tryTakeTMVar   = WrappedSTM .  tryTakeTMVar
    putTMVar       = WrappedSTM .: putTMVar
    tryPutTMVar    = WrappedSTM .: tryPutTMVar
    readTMVar      = WrappedSTM .  readTMVar
    tryReadTMVar   = WrappedSTM .  tryReadTMVar
    swapTMVar      = WrappedSTM .: swapTMVar
    isEmptyTMVar   = WrappedSTM .  isEmptyTMVar

    type TQueue (ExceptT e m) = TQueue m
    newTQueue      = WrappedSTM newTQueue
    readTQueue     = WrappedSTM .  readTQueue
    tryReadTQueue  = WrappedSTM .  tryReadTQueue
    peekTQueue     = WrappedSTM .  peekTQueue
    tryPeekTQueue  = WrappedSTM .  tryPeekTQueue
    flushTQueue    = WrappedSTM .  flushTQueue
    writeTQueue v  = WrappedSTM .  writeTQueue v
    isEmptyTQueue  = WrappedSTM .  isEmptyTQueue
    unGetTQueue    = WrappedSTM .: unGetTQueue

    type TBQueue (ExceptT e m) = TBQueue m
    newTBQueue     = WrappedSTM .  newTBQueue
    readTBQueue    = WrappedSTM .  readTBQueue
    tryReadTBQueue = WrappedSTM .  tryReadTBQueue
    peekTBQueue    = WrappedSTM .  peekTBQueue
    tryPeekTBQueue = WrappedSTM .  tryPeekTBQueue
    flushTBQueue   = WrappedSTM .  flushTBQueue
    writeTBQueue   = WrappedSTM .: writeTBQueue
    lengthTBQueue  = WrappedSTM .  lengthTBQueue
    isEmptyTBQueue = WrappedSTM .  isEmptyTBQueue
    isFullTBQueue  = WrappedSTM .  isFullTBQueue
    unGetTBQueue   = WrappedSTM .: unGetTBQueue

    type TArray (ExceptT e m) = TArray m

    type TSem (ExceptT e m) = TSem m
    newTSem        = WrappedSTM .  newTSem
    waitTSem       = WrappedSTM .  waitTSem
    signalTSem     = WrappedSTM .  signalTSem
    signalTSemN    = WrappedSTM .: signalTSemN

    type TChan (ExceptT e m) = TChan m
    newTChan          = WrappedSTM    newTChan
    newBroadcastTChan = WrappedSTM    newBroadcastTChan
    dupTChan          = WrappedSTM .  dupTChan
    cloneTChan        = WrappedSTM .  cloneTChan
    readTChan         = WrappedSTM .  readTChan
    tryReadTChan      = WrappedSTM .  tryReadTChan
    peekTChan         = WrappedSTM .  peekTChan
    tryPeekTChan      = WrappedSTM .  tryPeekTChan
    writeTChan        = WrappedSTM .: writeTChan
    unGetTChan        = WrappedSTM .: unGetTChan
    isEmptyTChan      = WrappedSTM .  isEmptyTChan


instance (Monoid w, MonadSTM m) => MonadSTM (RWST r w s m) where
    type STM (RWST r w s m) = WrappedSTM RWS (r, w, s) m
    atomically = lift . atomically . runWrappedSTM

    type TVar (RWST r w s m) = TVar m
    newTVar        = WrappedSTM .  newTVar
    readTVar       = WrappedSTM .  readTVar
    writeTVar      = WrappedSTM .: writeTVar
    retry          = WrappedSTM    retry
    orElse         = WrappedSTM .: on orElse runWrappedSTM

    modifyTVar     = WrappedSTM .: modifyTVar
    modifyTVar'    = WrappedSTM .: modifyTVar'
    stateTVar      = WrappedSTM .: stateTVar
    swapTVar       = WrappedSTM .: swapTVar
    check          = WrappedSTM .  check

    type TMVar (RWST r w s m) = TMVar m
    newTMVar       = WrappedSTM .  newTMVar
    newEmptyTMVar  = WrappedSTM    newEmptyTMVar
    takeTMVar      = WrappedSTM .  takeTMVar
    tryTakeTMVar   = WrappedSTM .  tryTakeTMVar
    putTMVar       = WrappedSTM .: putTMVar
    tryPutTMVar    = WrappedSTM .: tryPutTMVar
    readTMVar      = WrappedSTM .  readTMVar
    tryReadTMVar   = WrappedSTM .  tryReadTMVar
    swapTMVar      = WrappedSTM .: swapTMVar
    isEmptyTMVar   = WrappedSTM .  isEmptyTMVar

    type TQueue (RWST r w s m) = TQueue m
    newTQueue      = WrappedSTM newTQueue
    readTQueue     = WrappedSTM .  readTQueue
    tryReadTQueue  = WrappedSTM .  tryReadTQueue
    peekTQueue     = WrappedSTM .  peekTQueue
    tryPeekTQueue  = WrappedSTM .  tryPeekTQueue
    flushTQueue    = WrappedSTM .  flushTQueue
    writeTQueue v  = WrappedSTM .  writeTQueue v
    isEmptyTQueue  = WrappedSTM .  isEmptyTQueue
    unGetTQueue    = WrappedSTM .: unGetTQueue

    type TBQueue (RWST r w s m) = TBQueue m
    newTBQueue     = WrappedSTM . newTBQueue
    readTBQueue    = WrappedSTM . readTBQueue
    tryReadTBQueue = WrappedSTM . tryReadTBQueue
    peekTBQueue    = WrappedSTM . peekTBQueue
    tryPeekTBQueue = WrappedSTM . tryPeekTBQueue
    flushTBQueue   = WrappedSTM . flushTBQueue
    writeTBQueue   = WrappedSTM .: writeTBQueue
    lengthTBQueue  = WrappedSTM . lengthTBQueue
    isEmptyTBQueue = WrappedSTM . isEmptyTBQueue
    isFullTBQueue  = WrappedSTM . isFullTBQueue
    unGetTBQueue   = WrappedSTM .: unGetTBQueue

    type TArray (RWST r w s m) = TArray m

    type TSem (RWST r w s m) = TSem m
    newTSem        = WrappedSTM .  newTSem
    waitTSem       = WrappedSTM .  waitTSem
    signalTSem     = WrappedSTM .  signalTSem
    signalTSemN    = WrappedSTM .: signalTSemN

    type TChan (RWST r w s m) = TChan m
    newTChan          = WrappedSTM    newTChan
    newBroadcastTChan = WrappedSTM    newBroadcastTChan
    dupTChan          = WrappedSTM .  dupTChan
    cloneTChan        = WrappedSTM .  cloneTChan
    readTChan         = WrappedSTM .  readTChan
    tryReadTChan      = WrappedSTM .  tryReadTChan
    peekTChan         = WrappedSTM .  peekTChan
    tryPeekTChan      = WrappedSTM .  tryPeekTChan
    writeTChan        = WrappedSTM .: writeTChan
    unGetTChan        = WrappedSTM .: unGetTChan
    isEmptyTChan      = WrappedSTM .  isEmptyTChan


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
