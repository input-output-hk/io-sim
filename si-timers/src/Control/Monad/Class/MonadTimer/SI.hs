{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Class.MonadTimer.SI
  ( -- * SI Timers API
    threadDelay
  , registerDelay
  , registerDelayCancellable
  , timeout
    -- * Auxiliary functions
  , diffTimeToMicrosecondsAsInt
  , microsecondsAsIntToDiffTime
    -- * Re-exports
  , DiffTime
  , MonadDelay
  , MonadFork
  , MonadMonotonicTime
  , MonadTime
  , MonadTimeout
  , MonadTimer
  , TimeoutState (..)
  ) where

import           Control.Concurrent.Class.MonadSTM
import           Control.Exception (assert)
import           Control.Monad (when)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer (MonadDelay, MonadTimer)
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Monad.Class.MonadTimer.NonStandard

import           Data.Foldable (traverse_)

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


-- | Thread delay.  When the delay is smaller than what `Int` can represent it
-- will use the `Control.Monad.Class.MonadTimer.threadDelay` (e.g. for the `IO`
-- monad it will use `Control.Concurrent.threadDelay`); otherwise it will
-- recursively call `Control.Monad.Class.MonadTimer.threadDelay`.
--
threadDelay :: forall m.
               ( MonadDelay m
               , MonadMonotonicTime m
               )
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


-- | Like 'GHC.Conc.registerDelay' but safe on 32-bit systems.  When the delay
-- is larger than what `Int` can represent it will fork a thread which will
-- write to the returned 'TVar' once the delay has passed.  When the delay is
-- small enough it will use the `MonadTimer`'s `registerDelay` (e.g. for `IO`
-- monad it will use the `GHC`'s `GHC.Conc.registerDelay`).
--
registerDelay :: ( MonadFork    m
                 , MonadMonotonicTime m
                 , MonadTimeout m
                 , MonadTimer   m
                 )
              => DiffTime -> m (TVar m Bool)
registerDelay d
    | d <= maxDelay =
      MonadTimer.registerDelay (diffTimeToMicrosecondsAsInt d)
    | otherwise =
      defaultRegisterDelay d
  where
    maxDelay :: DiffTime
    maxDelay = microsecondsAsIntToDiffTime maxBound


-- | A default implementation of `registerDelay` which supports delays longer
-- then `Int`; this is especially important on 32-bit systems where maximum
-- delay expressed in microseconds is around 35 minutes.
--
defaultRegisterDelay :: forall m.
                        ( MonadFork    m
                        , MonadMonotonicTime m
                        , MonadTimeout m
                        )
                     => DiffTime
                     -> m (TVar m Bool)
defaultRegisterDelay d = do
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
registerDelayCancellable :: forall m.
                            ( MonadFork    m
                            , MonadMonotonicTime m
                            , MonadTimeout m
                            )
                         => DiffTime
                         -> m (STM m TimeoutState, m ())

registerDelayCancellable d | d <= maxDelay = do
    t <- newTimeout (diffTimeToMicrosecondsAsInt d)
    return (readTimeout t, cancelTimeout t)
  where
    maxDelay :: DiffTime
    maxDelay = microsecondsAsIntToDiffTime maxBound

registerDelayCancellable d = do
    -- current time
    c <- getMonotonicTime
    -- timeout state
    v <- newTVarIO TimeoutPending
    -- current timer
    m <- newTVarIO Nothing
    tid <- forkIO $ go m v c (d `addTime` c)
    labelThread tid "delay-thread"
    let cancel = do
          t <- atomically $ do
            a <- readTVar v
            case a of
              TimeoutCancelled -> return Nothing
              TimeoutFired     -> return Nothing
              TimeoutPending   -> do
                writeTVar v TimeoutCancelled
                mt  <- readTVar m
                case mt of
                  Nothing -> retry
                  Just t  -> return (Just t)
          traverse_ cancelTimeout t
          killThread tid
    return (readTVar v, cancel)
  where
    maxDelay :: DiffTime
    maxDelay = microsecondsAsIntToDiffTime maxBound

    -- The timeout thread, it might be killed by an async exception. In this
    -- case the `cancel` action is responsible for updating state state of the
    -- timeout (held in `v`).
    go :: TVar m (Maybe (Timeout m))
       -> TVar m TimeoutState
       -> Time
       -> Time
       -> m ()
    go tv v c u | u `diffTime` c >= maxDelay = do
      t <- newTimeout maxBound
      _ <- atomically $ swapTVar tv $! Just t
      fired <- atomically $ awaitTimeout t
      when fired $ do
        c' <- getMonotonicTime
        go tv v c' u

    go tv v c u = do
      t <- newTimeout (diffTimeToMicrosecondsAsInt $ u `diffTime` c)
      _ <- atomically $ swapTVar tv $! Just t
      atomically $ do
        fired <- awaitTimeout t
        ts <- readTVar v
        when (fired && ts == TimeoutPending) $
          writeTVar v TimeoutFired

-- | Run IO action within a timeout.
--
-- TODO: not safe on 32-bit systems.
timeout :: MonadTimer m => DiffTime -> m a -> m (Maybe a)
timeout = MonadTimer.timeout . diffTimeToMicrosecondsAsInt
