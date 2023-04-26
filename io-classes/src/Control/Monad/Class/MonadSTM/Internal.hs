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

-- needed for `ReaderT` instance
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
    -- * MonadThrow aliases
  , throwSTM
  , catchSTM
    -- * Default implementations
    -- $default-implementations
    --
    -- ** Default 'TMVar' implementation
  , TMVarDefault (..)
  , newTMVarDefault
  , newEmptyTMVarDefault
  , takeTMVarDefault
  , tryTakeTMVarDefault
  , putTMVarDefault
  , tryPutTMVarDefault
  , readTMVarDefault
  , tryReadTMVarDefault
  , swapTMVarDefault
  , isEmptyTMVarDefault
  , labelTMVarDefault
  , traceTMVarDefault
    -- ** Default 'TBQueue' implementation
  , TQueueDefault (..)
  , newTQueueDefault
  , writeTQueueDefault
  , readTQueueDefault
  , tryReadTQueueDefault
  , isEmptyTQueueDefault
  , peekTQueueDefault
  , tryPeekTQueueDefault
  , flushTQueueDefault
  , unGetTQueueDefault
  , labelTQueueDefault
    -- ** Default 'TBQueue' implementation
  , TBQueueDefault (..)
  , newTBQueueDefault
  , writeTBQueueDefault
  , readTBQueueDefault
  , tryReadTBQueueDefault
  , peekTBQueueDefault
  , tryPeekTBQueueDefault
  , isEmptyTBQueueDefault
  , isFullTBQueueDefault
  , lengthTBQueueDefault
  , flushTBQueueDefault
  , unGetTBQueueDefault
  , labelTBQueueDefault
    -- ** Default 'TArray' implementation
  , TArrayDefault (..)
    -- ** Default 'TSem' implementation
  , TSemDefault (..)
  , newTSemDefault
  , waitTSemDefault
  , signalTSemDefault
  , signalTSemNDefault
  , labelTSemDefault
    -- ** Default 'TChan' implementation
  , TChanDefault (..)
  , newTChanDefault
  , newBroadcastTChanDefault
  , writeTChanDefault
  , readTChanDefault
  , tryReadTChanDefault
  , peekTChanDefault
  , tryPeekTChanDefault
  , dupTChanDefault
  , unGetTChanDefault
  , isEmptyTChanDefault
  , cloneTChanDefault
  , labelTChanDefault
  ) where

import           Prelude hiding (read)

import qualified Control.Concurrent.STM.TArray as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TSem as STM
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (unless, when)
import qualified Control.Monad.STM as STM

import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans (lift)

import qualified Control.Monad.Class.MonadThrow as MonadThrow

import           Control.Exception
import           Data.Array (Array, bounds)
import qualified Data.Array as Array
import           Data.Array.Base (IArray (numElements), MArray (..),
                     arrEleBottom, listArray, unsafeAt)
import           Data.Foldable (traverse_)
import           Data.Ix (Ix, rangeSize)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           GHC.Stack
import           Numeric.Natural (Natural)


-- $default-implementations
--
-- The default implementations are based on a `TVar` defined in the class.  They
-- are tailored towards `IOSim` rather than instances which would like to derive
-- from `IO` or monad transformers.


-- | The STM primitives parametrised by a monad `m`.
--
class (Monad m, Monad (STM m)) => MonadSTM m where
  -- | The STM monad.
  type STM  m = (stm :: Type -> Type)  | stm -> m
  -- | Atomically run an STM computation.
  --
  -- See `STM.atomically`.
  atomically :: HasCallStack => STM m a -> m a

  -- | A type of a 'TVar'.
  --
  -- See `STM.TVar'.
  type TVar m  :: Type -> Type

  newTVar      :: a -> STM m (TVar m a)
  readTVar     :: TVar m a -> STM m a
  writeTVar    :: TVar m a -> a -> STM m ()
  -- | See `STM.retry`.
  retry        :: STM m a
  -- | See `STM.orElse`.
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

  -- | See `STM.check`.
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

  newTVarIO           = atomically . newTVar
  readTVarIO          = atomically . readTVar
  newTMVarIO          = atomically . newTMVar
  newEmptyTMVarIO     = atomically   newEmptyTMVar
  newTQueueIO         = atomically   newTQueue
  newTBQueueIO        = atomically . newTBQueue
  newTChanIO          = atomically   newTChan
  newBroadcastTChanIO = atomically   newBroadcastTChan



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


-- | Labelled `TVar`s & friends.
--
-- The `IO` instances is no-op, the `IOSim` instance enhances simulation trace.
-- This is very useful when analysing low lever concurrency issues (e.g.
-- deadlocks, livelocks etc).
--
class MonadSTM m
   => MonadLabelledSTM m where
  -- | Name a `TVar`.
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


-- | This type class is indented for
-- ['io-sim'](https://hackage.haskell.org/package/io-sim), where one might want
-- to access a 'TVar' in the underlying 'ST' monad.
--
class ( MonadSTM m
      , Monad (InspectMonad m)
      )
    => MonadInspectSTM m where
    type InspectMonad m :: Type -> Type
    -- | Return the value of a `TVar` as an `InspectMonad` computation.
    --
    -- `inspectTVar` is useful if the value of a `TVar` observed by `traceTVar`
    -- contains other `TVar`s.
    inspectTVar  :: proxy m -> TVar  m a -> InspectMonad m a
    -- | Return the value of a `TMVar` as an `InspectMonad` computation.
    inspectTMVar :: proxy m -> TMVar m a -> InspectMonad m (Maybe a)
    -- TODO: inspectTQueue, inspectTBQueue

instance MonadInspectSTM IO where
    type InspectMonad IO = IO
    inspectTVar  _ = readTVarIO
    -- issue #3198: tryReadTMVarIO
    inspectTMVar _ = atomically . tryReadTMVar


-- | A GADT which instructs how to trace the value.  The 'traceDynamic' will
-- use dynamic tracing, e.g. "Control.Monad.IOSim.traceM"; while 'traceString'
-- will be traced with 'EventSay'.  The `IOSim`s dynamic tracing allows to
-- recover the value from the simulation trace (see
-- "Control.Monad.IOSim.selectTraceEventsDynamic").
--
data TraceValue where
    TraceValue :: forall tr. Typeable tr
               => { traceDynamic :: Maybe tr
                  , traceString  :: Maybe String
                  }
               -> TraceValue


-- | Use only a dynamic tracer.
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
  {-# MINIMAL traceTVar, traceTQueue, traceTBQueue #-}

  -- | Construct a trace output out of previous & new value of a 'TVar'.  The
  -- callback is called whenever an stm transaction which modifies the 'TVar' is
  -- committed.
  --
  -- This is supported by 'IOSim' (and 'IOSimPOR'); 'IO' has a trivial instance.
  --
  -- The simplest example is:
  -- 
  -- >
  -- > traceTVar (Proxy @m) tvar (\_ -> TraceString . show)
  -- >
  --
  -- Note that the interpretation of `TraceValue` depends on the monad `m`
  -- itself (see 'TraceValue').
  --
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

newEmptyTMVarDefault :: MonadSTM m => STM m (TMVarDefault m a)
newEmptyTMVarDefault = do
  t <- newTVar Nothing
  return (TMVar t)

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
-- ReaderT instance
--


-- | The underlying stm monad is also transformed.
--
instance MonadSTM m => MonadSTM (ReaderT r m) where
    type STM (ReaderT r m) = ReaderT r (STM m)
    atomically (ReaderT stm) = ReaderT $ \r -> atomically (stm r)

    type TVar (ReaderT r m) = TVar m
    newTVar        = lift .  newTVar
    readTVar       = lift .  readTVar
    writeTVar      = lift .: writeTVar
    retry          = lift    retry
    orElse (ReaderT a) (ReaderT b) = ReaderT $ \r -> a r `orElse` b r

    modifyTVar     = lift .: modifyTVar
    modifyTVar'    = lift .: modifyTVar'
    stateTVar      = lift .: stateTVar
    swapTVar       = lift .: swapTVar
    check          = lift  . check

    type TMVar (ReaderT r m) = TMVar m
    newTMVar       = lift .  newTMVar
    newEmptyTMVar  = lift    newEmptyTMVar
    takeTMVar      = lift .  takeTMVar
    tryTakeTMVar   = lift .  tryTakeTMVar
    putTMVar       = lift .: putTMVar
    tryPutTMVar    = lift .: tryPutTMVar
    readTMVar      = lift .  readTMVar
    tryReadTMVar   = lift .  tryReadTMVar
    swapTMVar      = lift .: swapTMVar
    isEmptyTMVar   = lift .  isEmptyTMVar

    type TQueue (ReaderT r m) = TQueue m
    newTQueue      = lift newTQueue
    readTQueue     = lift .  readTQueue
    tryReadTQueue  = lift .  tryReadTQueue
    peekTQueue     = lift .  peekTQueue
    tryPeekTQueue  = lift .  tryPeekTQueue
    flushTQueue    = lift .  flushTQueue
    writeTQueue v  = lift .  writeTQueue v
    isEmptyTQueue  = lift .  isEmptyTQueue
    unGetTQueue    = lift .: unGetTQueue

    type TBQueue (ReaderT r m) = TBQueue m
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

    type TArray (ReaderT r m) = TArray m

    type TSem (ReaderT r m) = TSem m
    newTSem        = lift .  newTSem
    waitTSem       = lift .  waitTSem
    signalTSem     = lift .  signalTSem
    signalTSemN    = lift .: signalTSemN

    type TChan (ReaderT r m) = TChan m
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
