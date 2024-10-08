{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Control.Concurrent.Class.MonadMVar where

import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Data.Bifoldable (bifoldMap)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Data.Maybe (isNothing)
import Data.Monoid (All (..))

import Control.Monad.IOSim

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Control.Concurrent.Class.MonadMVar"
    [ testGroup "putMVar"
      [ testProperty "fairness (IOSim)" prop_putMVar_fairness_sim
      , testCase "blocks on a full MVar (IOSim)"
        unit_putMVar_blocks_on_full_sim
      , testCase "blocks on a full MVar (IO)"
        unit_putMVar_blocks_on_full_io
      ]
    , testGroup "takeMVar"
      [ testProperty "fairness (IOSim)" prop_takeMVar_fairness_sim
      , testCase "blocks on an empty MVar (IOSim)"
        unit_takeMVar_blocks_on_empty_sim
      , testCase "blocks on an empty MVar (IO)"
        unit_takeMVar_blocks_on_empty_io
      ]
    , testGroup "tryTakeMVar"
      [ testCase "does not block on an empty MVar (IOSim)"
        unit_tryTakeMVar_empty
      , testCase "does not block on a full MVar (IOSim)"
        unit_tryTakeMVar_full
      , testCase "return value on an empty MVar (IOSim)"
        unit_tryTakeMVar_return_empty_sim
      , testCase "return value on an full MVar (IOSim)"
        unit_tryTakeMVar_return_full_sim
      ]
    , testGroup "tryPutMVar"
      [ testCase "does not block on an empty MVar (IOSim)"
        unit_tryPutMVar_empty
      , testCase "does not block on a full MVar (IOSim)"
        unit_tryPutMVar_full
      , testCase "return value on an empty MVar (IOSim)"
        unit_tryPutMVar_return_empty_sim
      , testCase "return value on an full MVar (IOSim)"
        unit_tryPutMVar_return_full_sim
      ]
    , testGroup "isEmptyMVar"
      [ testCase "empty MVar is empty"    unit_isEmptyMVar_empty_sim
      , testCase "full MVar is not empty" unit_isEmptyMVar_full_sim
      ]
    , testProperty "takeMVar is exception safe" prop_takeMVar_exception_safe
    ]


--
-- putMVar
--

-- | Check that 'takeMVar' is fair.  This is test is only designed for 'IOSim'
-- as it relies on its thread scheduling and determinism.
--
putMVar_fairness_property
  :: forall m.
     ( MonadAsync m
     , MonadDelay m
     , MonadMVar  m
     )
  => Int -- ^ number of threads
  -> m Bool
putMVar_fairness_property n = do
    v  <- newEmptyMVar
    traverse_ (\a -> async $ do threadDelay 0.01
                                putMVar v a)
              [1..n]
    threadDelay 0.02
    results <- sequence (replicate n (takeMVar v))
    return $ results == [1..n]

prop_putMVar_fairness_sim :: Positive (Small Int)
                          -> Property
prop_putMVar_fairness_sim (Positive (Small n)) =
    let trace = runSimTrace (putMVar_fairness_property n)
    in counterexample (ppTrace trace)
     $ case traceResult False trace of
        Left err -> counterexample (show err) False
        Right a  -> property a


unit_putMVar_blocks_on_full
  :: ( MonadFork  m
     , MonadDelay m
     , MonadMVar  m
     )
  => m Bool
unit_putMVar_blocks_on_full = do
    start <- getMonotonicTime
    let delta = 0.01
    v <- newMVar ()
    _ <- forkIO $ threadDelay delta
               >> takeMVar v
               $> ()
    putMVar v ()
    end <- getMonotonicTime
    return (end `diffTime` start >= delta)

unit_putMVar_blocks_on_full_sim :: Assertion
unit_putMVar_blocks_on_full_sim = assertBool "did not block on an full MVar" $
    runSimOrThrow unit_putMVar_blocks_on_full

unit_putMVar_blocks_on_full_io :: Assertion
unit_putMVar_blocks_on_full_io =
    unit_putMVar_blocks_on_full >>= assertBool "did not block on an full MVar"


--
-- takeMVar
--

-- | Check that 'takeMVar' is fair.  This is test is only designed for 'IOSim'
-- as it relies on its thread scheduling and determinism.
--
takeMVar_fairness_property
  :: forall m.
     ( MonadAsync m
     , MonadDelay m
     , MonadMVar  m
     , Eq (Async m Int)
     )
  => Int -- ^ number of threads
  -> m Property
takeMVar_fairness_property n = do
    v  <- newEmptyMVar
    ts <- sequence $ replicate n (async $ takeMVar v)
    threadDelay 0.01
    traverse_ (putMVar v) [1..n]
    results <- waitAll ts
    return $ results === [1..n]

prop_takeMVar_fairness_sim :: Positive (Small Int)
                           -> Property
prop_takeMVar_fairness_sim (Positive (Small n)) =
    runSimOrThrow (takeMVar_fairness_property n)


unit_takeMVar_blocks_on_empty
  :: ( MonadFork  m
     , MonadDelay m
     , MonadMVar  m
     )
  => m Bool
unit_takeMVar_blocks_on_empty = do
    start <- getMonotonicTime
    let delta = 0.01
    v <- newEmptyMVar
    _ <- forkIO $ threadDelay delta
               >> putMVar v ()
    takeMVar v
    end <- getMonotonicTime
    return (end `diffTime` start >= delta)

unit_takeMVar_blocks_on_empty_sim :: Assertion
unit_takeMVar_blocks_on_empty_sim = assertBool "did not block on an empty MVar" $ runSimOrThrow unit_takeMVar_blocks_on_empty

unit_takeMVar_blocks_on_empty_io :: Assertion
unit_takeMVar_blocks_on_empty_io =
    unit_takeMVar_blocks_on_empty >>= assertBool "did not block on an empty MVar"

--
-- tryTakeMVar
--


-- | Check that `IOSim`'s `tryTakeMVar` is non blocking.
--
tryTakeMVar_non_blocking_property
  :: Bool -> Bool
tryTakeMVar_non_blocking_property isEmpty =
    validateTrace $ runSimTrace $ do
      v <- if isEmpty
           then newEmptyMVar
           else newMVar ()
      void $ tryTakeMVar v
  where
    validateTrace :: SimTrace a -> Bool
    validateTrace = getAll . bifoldMap (const (All True))
                                       (\ev -> case seType ev of
                                           EventTxBlocked {} -> All False
                                           _                 -> All True)

unit_tryTakeMVar_empty :: Assertion
unit_tryTakeMVar_empty = assertBool "blocked on an empty MVar" $
    tryTakeMVar_non_blocking_property False

unit_tryTakeMVar_full :: Assertion
unit_tryTakeMVar_full = assertBool "blocked on an empty MVar" $
    tryTakeMVar_non_blocking_property True


tryTakeMVar_return_value
  :: MonadMVar m
  => Bool
  -> m Bool
tryTakeMVar_return_value isEmpty =
    do v <- if isEmpty
            then newEmptyMVar
            else newMVar ()
       a <- tryTakeMVar v
       return $ isNothing a == isEmpty

unit_tryTakeMVar_return_empty_sim :: Assertion
unit_tryTakeMVar_return_empty_sim =
    assertBool "tryTakeMVar on an empty should return result" $
    runSimOrThrow (tryTakeMVar_return_value True)

unit_tryTakeMVar_return_full_sim :: Assertion
unit_tryTakeMVar_return_full_sim =
    assertBool "tryTakeMVar on an full should return result" $
    runSimOrThrow (tryTakeMVar_return_value False)

--
-- tryPutMVar
--

-- | Check that `IOSim`'s `tryPutMVar` is non blocking.
--
tryPutMVar_non_blocking_property
  :: Bool -> Bool
tryPutMVar_non_blocking_property isEmpty =
    validateTrace $ runSimTrace $ do
      v <- if isEmpty
           then newEmptyMVar
           else newMVar ()
      void $ tryPutMVar v ()
  where
    validateTrace :: SimTrace a -> Bool
    validateTrace = getAll . bifoldMap (const (All True))
                                       (\ev -> case seType ev of
                                           EventTxBlocked {} -> All False
                                           _                 -> All True)

unit_tryPutMVar_empty :: Assertion
unit_tryPutMVar_empty = assertBool "blocked on an empty MVar" $
    tryPutMVar_non_blocking_property False

unit_tryPutMVar_full :: Assertion
unit_tryPutMVar_full = assertBool "blocked on an empty MVar" $
    tryPutMVar_non_blocking_property True


tryPutMVar_return_value
  :: forall m.
     MonadMVar m
  => Bool
  -> m Bool
tryPutMVar_return_value isEmpty = do
    v :: MVar m ()
      <- if isEmpty
         then newEmptyMVar
         else newMVar ()
    a <- tryPutMVar v ()
    return $ a == isEmpty

unit_tryPutMVar_return_empty_sim :: Assertion
unit_tryPutMVar_return_empty_sim =
    assertBool "tryPutMVar on an empty should return result" $
    runSimOrThrow (tryPutMVar_return_value True)

unit_tryPutMVar_return_full_sim :: Assertion
unit_tryPutMVar_return_full_sim =
    assertBool "tryPutMVar on an full should return result" $
    runSimOrThrow (tryPutMVar_return_value False)

--
-- isEmptyMVar
--

prop_isEmptyMVar
  :: forall m. MonadMVar m
  => Bool
  -> m Bool
prop_isEmptyMVar isEmpty = do
    v :: MVar m ()
      <- if isEmpty
         then newEmptyMVar
         else newMVar ()
    (isEmpty ==) <$> isEmptyMVar v

unit_isEmptyMVar_empty_sim :: Assertion
unit_isEmptyMVar_empty_sim =
    assertBool "empty mvar must be empty" $
    runSimOrThrow (prop_isEmptyMVar True)

unit_isEmptyMVar_full_sim :: Assertion
unit_isEmptyMVar_full_sim =
    assertBool "full mvar must not be empty" $
    runSimOrThrow (prop_isEmptyMVar False)

--
-- takeMVar is exception safe
--
prop_takeMVar_exception_safe :: Property
prop_takeMVar_exception_safe =
  exploreSimTrace id (do
      exploreRaces
      mv <- newMVar (0 :: Int)
      t1 <- async $ void $ withMVar mv (\v -> pure (v + 1, ()))
      t2 <- async $ void $ do
        _ <- withMVar mv (\v -> pure (v + 1, ()))
        withMVar mv (\v -> pure (v + 1, ()))
      t3 <- async $ cancel t1
      wait t3
      wait t2
      wait t1
  ) (\_ trace ->
       case traceResult False trace of
         Left FailureDeadlock{} ->
           counterexample (ppTrace trace) $ property False
         _ -> property True
    )

--
-- Utils
--

waitAll :: forall m.
           ( MonadAsync m
           , Eq (Async m Int)
           )
        => [Async m Int] -> m [Int]
waitAll = go []
  where
    go :: [Int] -> [Async m Int] -> m [Int]
    go as ts = do
      (t, a) <- waitAny ts
      let ts' = filter (/= t) ts
      case ts' of
        [] -> return (reverse (a : as))
        _  -> go (a : as) ts'
