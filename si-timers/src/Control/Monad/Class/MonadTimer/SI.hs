{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Class.MonadTimer.SI
  ( -- * Timers API
    threadDelay
  , registerDelay
  , registerDelayCancellable
  , timeout
    -- * re-exports
  , DiffTime
  , MonadDelay
  , MonadFork
  , MonadMonotonicTime
  , MonadTime
  , MonadTimer
  , TimeoutState (..)
    -- * auxiliary functions
  , diffTimeToMicrosecondsAsInt
  , microsecondsAsIntToDiffTime
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


registerDelay :: ( MonadFork m
                 , MonadTime m
                 , MonadTimer m
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
-- then `Int`; this is especially important on 32-bit architectures where
-- maximum delay expressed in microseconds is around 35 minutes.
--
defaultRegisterDelay :: forall m.
                        ( MonadFork  m
                        , MonadMonotonicTime m
                        , MonadTimer m
                        )
                     => DiffTime
                     -> m (TVar m Bool)
defaultRegisterDelay d = do
    c <- getMonotonicTime
    v <- atomically $ newTVar False
    _ <- forkIO $ go v c (d `addTime` c)
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


registerDelayCancellable :: forall m.
                            ( MonadFork  m
                            , MonadTime  m
                            , MonadTimer m
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
-- TODO: not 32-bit safe
timeout :: MonadTimer m => DiffTime -> m a -> m (Maybe a)
timeout = MonadTimer.timeout . diffTimeToMicrosecondsAsInt
