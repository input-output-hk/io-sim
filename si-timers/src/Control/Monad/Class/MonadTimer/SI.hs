{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Class.MonadTimer.SI
  ( -- * Type classes
    MonadDelay (..)
  , MonadTimer (..)
    -- * Auxiliary functions
  , diffTimeToMicrosecondsAsInt
  , microsecondsAsIntToDiffTime
    -- * Re-exports
  , DiffTime
  , MonadFork
  , MonadMonotonicTime
  , MonadTime
  , TimeoutState (..)
    -- * Default implementations
  , defaultRegisterDelay
  , defaultRegisterDelayCancellable
  ) where

import           Control.Concurrent.Class.MonadSTM
import           Control.Exception (assert)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTime.SI
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Monad.Class.MonadTimer.NonStandard (TimeoutState (..))
import qualified Control.Monad.Class.MonadTimer.NonStandard as NonStandard

import           Control.Monad.Reader

import           Data.Bifunctor (bimap)
import           Data.Functor (($>))
import           Data.Time.Clock (diffTimeToPicoseconds)



-- | Convert 'DiffTime' in seconds to microseconds represented by an 'Int'.
--
-- Note that on 32bit systems it can only represent `2^31-1` seconds, which is
-- only ~35 minutes.
diffTimeToMicrosecondsAsInt :: DiffTime -> Int
diffTimeToMicrosecondsAsInt d =
    let usec :: Integer
        usec = diffTimeToPicoseconds d `div` 1_000_000 in
    assert (usec <= fromIntegral (maxBound :: Int)) $
    fromIntegral usec


-- | Convert time in microseconds in 'DiffTime' (measured in seconds).
--
microsecondsAsIntToDiffTime :: Int -> DiffTime
microsecondsAsIntToDiffTime = (/ 1_000_000) . fromIntegral

class ( MonadTimer.MonadDelay m
      , MonadMonotonicTime m
      ) => MonadDelay m where
  threadDelay :: DiffTime -> m ()

-- | Thread delay.  When the delay is smaller than what `Int` can represent it
-- will use the `Control.Monad.Class.MonadTimer.threadDelay` (e.g. for the `IO`
-- monad it will use `Control.Concurrent.threadDelay`); otherwise it will
-- recursively call `Control.Monad.Class.MonadTimer.threadDelay`.
--
instance MonadDelay IO where
  threadDelay :: forall m.
                 MonadDelay m
              => DiffTime -> m ()
  threadDelay d | d <= maxDelay =
      MonadTimer.threadDelay (diffTimeToMicrosecondsAsInt d)
    where
      maxDelay :: DiffTime
      maxDelay = microsecondsAsIntToDiffTime maxBound

  threadDelay d = do
      c <- getMonotonicTime
      let u = d `addTime` c
      go c u
    where
      maxDelay :: DiffTime
      maxDelay = microsecondsAsIntToDiffTime maxBound

      go :: Time -> Time -> m ()
      go c u = do
        if d' >= maxDelay
          then do
            MonadTimer.threadDelay maxBound
            c' <- getMonotonicTime
            go  c' u
          else
            MonadTimer.threadDelay (diffTimeToMicrosecondsAsInt d')
        where
          d' = u `diffTime` c

instance MonadDelay m => MonadDelay (ReaderT r m) where
  threadDelay = lift . threadDelay

class ( MonadTimer.MonadTimer m
      , MonadMonotonicTime m
      ) => MonadTimer m where

  -- | A register delay function which safe on 32-bit systems.
  registerDelay            :: DiffTime -> m (TVar m Bool)

  -- | A cancellable register delay which is safe on 32-bit systems and efficient
  -- for delays smaller than what `Int` can represent (especially on systems which
  -- support native timer manager).
  --
  registerDelayCancellable :: DiffTime -> m (STM m TimeoutState, m ())

  -- | A timeout function.
  --
  -- TODO: 'IO' instance is not safe on 32-bit systems.
  timeout                  :: DiffTime -> m a -> m (Maybe a)


-- | A default implementation of `registerDelay` which supports delays longer
-- then `Int`; this is especially important on 32-bit systems where maximum
-- delay expressed in microseconds is around 35 minutes.
--
defaultRegisterDelay :: forall m timeout.
                        ( MonadFork m
                        , MonadMonotonicTime m
                        , MonadSTM m
                        )
                     => NonStandard.NewTimeout m timeout
                     -> NonStandard.AwaitTimeout m timeout
                     -> DiffTime
                     -> m (TVar m Bool)
defaultRegisterDelay newTimeout awaitTimeout d = do
    c <- getMonotonicTime
    v <- atomically $ newTVar False
    tid <- forkIO $ go v c (d `addTime` c)
    labelThread tid "delay-thread"
    return v
  where
    maxDelay :: DiffTime
    maxDelay = microsecondsAsIntToDiffTime maxBound

    go :: TVar m Bool -> Time -> Time -> m ()
    go v c u | u `diffTime` c >= maxDelay = do
      _ <- newTimeout maxBound >>= atomically . awaitTimeout
      c' <- getMonotonicTime
      go v c' u

    go v c u = do
      t <- newTimeout (diffTimeToMicrosecondsAsInt $ u `diffTime` c)
      atomically $ do
        _ <- awaitTimeout t
        writeTVar v True


-- | A cancellable register delay which is safe on 32-bit systems and efficient
-- for delays smaller than what `Int` can represent (especially on systems which
-- support native timer manager).
--
defaultRegisterDelayCancellable :: forall m timeout.
                                   ( MonadFork m
                                   , MonadMonotonicTime m
                                   , MonadSTM m
                                   )
                                => NonStandard.NewTimeout    m timeout
                                -> NonStandard.ReadTimeout   m timeout
                                -> NonStandard.CancelTimeout m timeout
                                -> NonStandard.AwaitTimeout  m timeout
                                -> DiffTime
                                -> m (STM m TimeoutState, m ())

defaultRegisterDelayCancellable newTimeout readTimeout cancelTimeout _awaitTimeout d | d <= maxDelay = do
    t <- newTimeout (diffTimeToMicrosecondsAsInt d)
    return (readTimeout t, cancelTimeout t)
  where
    maxDelay :: DiffTime
    maxDelay = microsecondsAsIntToDiffTime maxBound

defaultRegisterDelayCancellable newTimeout _readTimeout _cancelTimeout awaitTimeout d = do
    -- current time
    c <- getMonotonicTime
    -- timeout state
    v <- newTVarIO TimeoutPending
    tid <- forkIO $ go v c (d `addTime` c)
    labelThread tid "delay-thread"
    let cancel = atomically $ readTVar v >>= \case
          TimeoutCancelled -> return ()
          TimeoutFired     -> return ()
          TimeoutPending   -> writeTVar v TimeoutCancelled
    return (readTVar v, cancel)
  where
    maxDelay :: DiffTime
    maxDelay = microsecondsAsIntToDiffTime maxBound

    go :: TVar m TimeoutState
       -> Time
       -> Time
       -> m ()
    go v c u | u `diffTime` c >= maxDelay = do
      t <- newTimeout maxBound
      ts <- atomically $ do
        (readTVar v >>= \case
           a@TimeoutCancelled -> return a
           TimeoutFired       -> error "registerDelayCancellable: invariant violation!"
           TimeoutPending     -> retry)
        `orElse`
        -- the overall timeout is still pending when 't' fires
        (awaitTimeout t $> TimeoutPending)
      case ts of
        TimeoutPending -> do
          c' <- getMonotonicTime
          go v c' u
        _ -> return ()

    go v c u = do
      t <- newTimeout (diffTimeToMicrosecondsAsInt $ u `diffTime` c)
      atomically $ do
        ts <- (readTVar v >>= \case
                 a@TimeoutCancelled -> return a
                 TimeoutFired       -> error "registerDelayCancellable: invariant violation!"
                 TimeoutPending     -> retry)
              `orElse`
              -- the overall timeout fires when 't' fires
              (awaitTimeout t $> TimeoutFired)
        case ts of
          TimeoutFired -> writeTVar v TimeoutFired
          _            -> return ()


-- | Like 'GHC.Conc.registerDelay' but safe on 32-bit systems.  When the delay
-- is larger than what `Int` can represent it will fork a thread which will
-- write to the returned 'TVar' once the delay has passed.  When the delay is
-- small enough it will use the `MonadTimer`'s `registerDelay` (e.g. for `IO`
-- monad it will use the `GHC`'s `GHC.Conc.registerDelay`).
--
-- TODO: 'timeout' not safe on 32-bit systems.
instance MonadTimer IO where
  registerDelay d
      | d <= maxDelay =
        MonadTimer.registerDelay (diffTimeToMicrosecondsAsInt d)
      | otherwise =
        defaultRegisterDelay
          NonStandard.newTimeout
          NonStandard.awaitTimeout
          d
    where
      maxDelay :: DiffTime
      maxDelay = microsecondsAsIntToDiffTime maxBound

  registerDelayCancellable =
    defaultRegisterDelayCancellable
      NonStandard.newTimeout
      NonStandard.readTimeout
      NonStandard.cancelTimeout
      NonStandard.awaitTimeout

  timeout = MonadTimer.timeout . diffTimeToMicrosecondsAsInt

instance MonadTimer m => MonadTimer (ReaderT r m) where
  registerDelay            = lift . registerDelay
  registerDelayCancellable = fmap (bimap lift lift) . lift . registerDelayCancellable
  timeout d f              = ReaderT $ \r -> timeout d (runReaderT f r)
