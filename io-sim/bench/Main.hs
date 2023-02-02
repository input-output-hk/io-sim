{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.Class.MonadSTM
import           Control.Monad (forever, replicateM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           Criterion
import           Criterion.Main

import           Control.Exception (AsyncException (..))
import           Data.Foldable (traverse_)


--
-- timers, delays, timeouts
--

prop_threadDelay :: forall m. MonadDelay m => m ()
prop_threadDelay = threadDelay 1

prop_registerDelay :: forall m. MonadTimer m => m ()
prop_registerDelay = registerDelay 1 >>= \v -> atomically (readTVar v >>= check)

prop_timeout_fail :: forall m. MonadTimer m => m (Maybe ())
prop_timeout_fail = timeout 1 (threadDelay 2)

prop_timeout_succeed :: forall m. MonadTimer m => m (Maybe ())
prop_timeout_succeed = timeout 2 (threadDelay 1)

prop_timeout_race :: forall m. MonadTimer m => m (Maybe ())
prop_timeout_race = timeout 1 (threadDelay 1)


--
-- threads, async
--

prop_threads :: forall m. (MonadFork m, MonadDelay m, MonadSay m) => Int -> m ()
prop_threads n = do
    threads <- replicateM n (forkIO $ threadDelay 2
                                   >> say ""
                            )
    threadDelay 1
    traverse_ (\tid -> throwTo tid ThreadKilled) threads


prop_async :: forall m. (MonadAsync m, MonadDelay m, MonadSay m) => Int -> m ()
prop_async n = do
    threads <- replicateM n (async $ threadDelay 1
                                  >> say ""
                            )
    traverse_ wait threads

prop_threadDelay_bottleneck :: forall m. (MonadTimer m, MonadSay m)
                            => m (Maybe ())
prop_threadDelay_bottleneck =
  timeout 1000000 $ do
    forever $ do
      threadDelay 1
      say ""

main :: IO ()
main = defaultMain
    [ env (pure ()) $ \_ ->
      bgroup "delays"
      [ bench "threadDelay" $
        whnf id (runSimOrThrow prop_threadDelay)
      , bench "registerDelay" $
        whnf id (runSimOrThrow prop_registerDelay)
      , bgroup "timeout"
        [ bench "fail" $
          whnf id (runSimOrThrow prop_timeout_fail)
        , bench "succeed" $
          whnf id (runSimOrThrow prop_timeout_succeed)
        , bench "race" $
          whnf id (runSimOrThrow prop_timeout_race)
        ]
      ]
    ,
      bgroup "threads"
      [ env (pure 50) $ \n ->
        bgroup "50"
        [ bench "async silent" $
          whnf id (runSimOrThrow (prop_async n))
        , bench "forkIO silent" $
          whnf id (runSimOrThrow (prop_threads n))
        , bench "threadDelay bottleneck silent" $
          whnf id (runSimOrThrow prop_threadDelay_bottleneck)
        , bench "async say" $
          nf id ( selectTraceEventsSay
                $ runSimTrace
                $ prop_async n)
        , bench "forkIO say" $
          nf id ( selectTraceEventsSay
                $ runSimTrace
                $ prop_threads n)
        , bench "threadDelay bottleneck say" $
          nf id ( selectTraceEventsSay
                $ runSimTrace
                $ prop_threadDelay_bottleneck)
        ]
      , env (pure 250) $ \n ->
        bgroup "250"
        [ bench "async" $
          whnf id (runSimOrThrow (prop_async n))
        , bench "forkIO" $
          whnf id (runSimOrThrow (prop_threads n))
        ]
      ]
    ]
