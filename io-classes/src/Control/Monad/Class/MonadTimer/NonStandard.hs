{-# LANGUAGE CPP                #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies       #-}

#if  defined(__GLASGOW_HASKELL__) && \
    !defined(mingw32_HOST_OS) && \
    !defined(__GHCJS__)
#define GHC_TIMERS_API
#endif

-- | A non-standard interface for timer api.
--
-- This module also provides a polyfill which allows to use timer api also on
-- non-threaded RTS regardless of the architecture \/ OS.
--
-- We use it to provide 'MonadTimer IO' instance.
--
-- You can expect we will deprecate it at some point (e.g. once GHC gets
-- a better support for timers especially across different OSes).
--
module Control.Monad.Class.MonadTimer.NonStandard
  ( MonadTimeout (..)
  , TimeoutState (..)
  , diffTimeToMicrosecondsAsInt
  , microsecondsAsIntToDiffTime
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import           Control.Exception (assert)
#ifndef GHC_TIMERS_API
import           Control.Monad (when)
#endif
import           Control.Monad.Class.MonadSTM
import qualified Control.Monad.STM as STM

import           Control.Monad.RWS (RWST (..))
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.State (StateT (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer (WriterT (..))

#ifdef GHC_TIMERS_API
import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)
#endif

import           Data.Kind (Type)
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds)


data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled
  deriving (Eq, Ord, Show)

class MonadSTM m => MonadTimeout m where
  -- | The type of the timeout handle, used with 'newTimeout', 'readTimeout',
  -- 'updateTimeout' and 'cancelTimeout'.
  --
  data Timeout m :: Type

  -- | Create a new timeout which will fire at the given time duration in
  -- the future.
  --
  -- The timeout will start in the 'TimeoutPending' state and either
  -- fire at or after the given time leaving it in the 'TimeoutFired' state,
  -- or it may be cancelled with 'cancelTimeout', leaving it in the
  -- 'TimeoutCancelled' state.
  --
  -- Timeouts /cannot/ be reset to the pending state once fired or cancelled
  -- (as this would be very racy). You should create a new timeout if you need
  -- this functionality.
  --
  newTimeout     :: DiffTime -> m (Timeout m)

  -- | Read the current state of a timeout. This does not block, but returns
  -- the current state. It is your responsibility to use 'retry' to wait.
  --
  -- Alternatively you may wish to use the convenience utility 'awaitTimeout'
  -- to wait for just the fired or cancelled outcomes.
  --
  -- You should consider the cancelled state if you plan to use 'cancelTimeout'.
  --
  readTimeout    :: Timeout m -> STM m TimeoutState

  -- Adjust when this timer will fire, to the given duration into the future.
  --
  -- It is safe to race this concurrently against the timer firing. It will
  -- have no effect if the timer fires first.
  --
  -- The new time can be before or after the original expiry time, though
  -- arguably it is an application design flaw to move timeouts sooner.
  --
  updateTimeout  :: Timeout m -> DiffTime -> m ()

  -- | Cancel a timeout (unless it has already fired), putting it into the
  -- 'TimeoutCancelled' state. Code reading and acting on the timeout state
  -- need to handle such cancellation appropriately.
  --
  -- It is safe to race this concurrently against the timer firing. It will
  -- have no effect if the timer fires first.
  --
  cancelTimeout  :: Timeout m -> m ()

  -- | Returns @True@ when the timeout is fired, or @False@ if it is cancelled.
  awaitTimeout   :: Timeout m -> STM m Bool
  awaitTimeout t  = do s <- readTimeout t
                       case s of
                         TimeoutPending   -> retry
                         TimeoutFired     -> return True
                         TimeoutCancelled -> return False

#ifdef GHC_TIMERS_API
instance MonadTimeout IO where
  data Timeout IO = TimeoutIO !(STM.TVar TimeoutState) !GHC.TimeoutKey

  readTimeout (TimeoutIO var _key) = STM.readTVar var

  newTimeout = \d -> do
      var <- STM.newTVarIO TimeoutPending
      mgr <- GHC.getSystemTimerManager
      key <- GHC.registerTimeout mgr (diffTimeToMicrosecondsAsInt d)
                                     (STM.atomically (timeoutAction var))
      return (TimeoutIO var key)
    where
      timeoutAction var = do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutFired
          TimeoutFired     -> error "MonadTimer(IO): invariant violation"
          TimeoutCancelled -> return ()

  -- In GHC's TimerManager this has no effect if the timer already fired.
  -- It is safe to race against the timer firing.
  updateTimeout (TimeoutIO _var key) d = do
      mgr <- GHC.getSystemTimerManager
      GHC.updateTimeout mgr key (diffTimeToMicrosecondsAsInt d)

  cancelTimeout (TimeoutIO var key) = do
      STM.atomically $ do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutCancelled
          TimeoutFired     -> return ()
          TimeoutCancelled -> return ()
      mgr <- GHC.getSystemTimerManager
      GHC.unregisterTimeout mgr key
#else
instance MonadTimeout IO where
  data Timeout IO = TimeoutIO !(STM.TVar (STM.TVar Bool)) !(STM.TVar Bool)

  readTimeout (TimeoutIO timeoutvarvar cancelvar) = do
    canceled <- STM.readTVar cancelvar
    fired    <- STM.readTVar =<< STM.readTVar timeoutvarvar
    case (canceled, fired) of
      (True, _)  -> return TimeoutCancelled
      (_, False) -> return TimeoutPending
      (_, True)  -> return TimeoutFired

  newTimeout d = do
    timeoutvar    <- STM.registerDelay (diffTimeToMicrosecondsAsInt d)
    timeoutvarvar <- STM.newTVarIO timeoutvar
    cancelvar     <- STM.newTVarIO False
    return (TimeoutIO timeoutvarvar cancelvar)

  updateTimeout (TimeoutIO timeoutvarvar _cancelvar) d = do
    timeoutvar' <- STM.registerDelay (diffTimeToMicrosecondsAsInt d)
    STM.atomically $ STM.writeTVar timeoutvarvar timeoutvar'

  cancelTimeout (TimeoutIO timeoutvarvar cancelvar) =
    STM.atomically $ do
      fired <- STM.readTVar =<< STM.readTVar timeoutvarvar
      when (not fired) $ STM.writeTVar cancelvar True
#endif


diffTimeToMicrosecondsAsInt :: DiffTime -> Int
diffTimeToMicrosecondsAsInt d =
    let usec :: Integer
        usec = diffTimeToPicoseconds d `div` 1_000_000 in
    -- Can only represent usec times that fit within an Int, which on 32bit
    -- systems means 2^31 usec, which is only ~35 minutes.
    assert (usec <= fromIntegral (maxBound :: Int)) $
    fromIntegral usec

microsecondsAsIntToDiffTime :: Int -> DiffTime
microsecondsAsIntToDiffTime = (/ 1_000_000) . fromIntegral

--
-- Transformer's instances
--

instance MonadTimeout m => MonadTimeout (ReaderT r m) where
  newtype Timeout (ReaderT r m) = TimeoutR { unTimeoutR :: Timeout m }
  newTimeout    = lift . fmap TimeoutR . newTimeout
  readTimeout   = WrappedSTM . readTimeout . unTimeoutR
  updateTimeout (TimeoutR t) d = lift $ updateTimeout t d
  cancelTimeout = lift . cancelTimeout . unTimeoutR

instance (Monoid w, MonadTimeout m) => MonadTimeout (WriterT w m) where
  newtype Timeout (WriterT w m) = TimeoutW { unTimeoutW :: Timeout m }
  newTimeout    = lift . fmap TimeoutW . newTimeout
  readTimeout   = WrappedSTM . readTimeout . unTimeoutW
  updateTimeout (TimeoutW t) d = lift $ updateTimeout t d
  cancelTimeout = lift . cancelTimeout . unTimeoutW

instance MonadTimeout m => MonadTimeout (StateT s m) where
  newtype Timeout (StateT s m) = TimeoutS { unTimeoutS :: Timeout m }
  newTimeout    = lift . fmap TimeoutS . newTimeout
  readTimeout   = WrappedSTM . readTimeout . unTimeoutS
  updateTimeout (TimeoutS t) d = lift $ updateTimeout t d
  cancelTimeout = lift . cancelTimeout . unTimeoutS

instance (Monoid w, MonadTimeout m) => MonadTimeout (RWST r w s m) where
  newtype Timeout (RWST r w s m) = TimeoutRWS { unTimeoutRWS :: Timeout m }
  newTimeout    = lift . fmap TimeoutRWS . newTimeout
  readTimeout   = WrappedSTM . readTimeout . unTimeoutRWS
  updateTimeout (TimeoutRWS t) d = lift $ updateTimeout t d
  cancelTimeout = lift . cancelTimeout . unTimeoutRWS
