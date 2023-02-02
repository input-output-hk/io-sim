{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Control.Monad.IOSim
  ( tests
  , TestThreadGraph (..)
  ) where

import           Data.Either (isLeft)
import           Data.Fixed (Micro)
import           Data.Foldable (foldl')
import           Data.Functor (($>))
import           Data.Time.Clock (picosecondsToDiffTime)

import           Control.Exception (ArithException (..))
import           Control.Monad
import           Control.Monad.Fix
import           System.IO.Error (ioeGetErrorString, isUserError)

import           Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.Class.MonadSTM.TVar as LazySTM
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           Test.Control.Monad.Utils
import           Test.Control.Monad.STM

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           GHC.Conc (ThreadStatus(..))


tests :: TestTree
tests =
  testGroup "IO simulator"
  [ testProperty "read/write graph (IO)"    prop_stm_graph_io
  , testProperty "read/write graph (IOSim)" (withMaxSuccess 1000 prop_stm_graph_sim)
  , testProperty "timers (IOSim)"           (withMaxSuccess 1000 prop_timers_ST)
  -- fails since we just use `threadDelay` to schedule timers in `IO`.
  , testProperty "timers (IO)"              (expectFailure prop_timers_IO)
  , testProperty "timeout (IOSim): no deadlock"
                                            prop_timeout_no_deadlock_Sim
  , testProperty "timeout (IO): no deadlock"
                                            prop_timeout_no_deadlock_IO
  , testProperty "threadId order (IOSim)"   (withMaxSuccess 1000 prop_threadId_order_order_Sim)
  , testProperty "forkIO order (IOSim)"     (withMaxSuccess 1000 prop_fork_order_ST)
  , testProperty "order (IO)"               (expectFailure prop_fork_order_IO)
  , testProperty "STM wakeup order"         prop_wakeup_order_ST
  , testGroup "throw/catch unit tests"
    [ testProperty "0" unit_catch_0
    , testProperty "1" unit_catch_1
    , testProperty "2" unit_catch_2
    , testProperty "3" unit_catch_3
    , testProperty "4" unit_catch_4
    , testProperty "5" unit_catch_5
    , testProperty "6" unit_catch_6
    ]
  , testGroup "masking state"
    [ testProperty "set (IO)"
    $ forall_masking_states unit_set_masking_state_IO
    , testProperty "set (IOSim)"
    $ forall_masking_states unit_set_masking_state_ST

    , testProperty "unmask (IO)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_unmask_IO ms ms'
    , testProperty "unmask (IOSim)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_unmask_ST ms ms'

    , testProperty "fork (IO)"
    $ forall_masking_states unit_fork_masking_state_IO
    , testProperty "fork (IOSim)"
    $ forall_masking_states unit_fork_masking_state_ST

    , testProperty "fork unmask (IO)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_fork_unmask_IO ms ms'
    , testProperty "fork unmask (IOSim)"
    $ forall_masking_states $ \ms  ->
      forall_masking_states $ \ms' -> unit_fork_unmask_ST ms ms'

    , testProperty "catch (IO)"
    $ forall_masking_states unit_catch_throwIO_masking_state_IO
    , testProperty "catch (IOSim)"
    $ forall_masking_states unit_catch_throwIO_masking_state_ST

    , testProperty "catch: throwTo (IO)"
    $ forall_masking_states unit_catch_throwTo_masking_state_IO
    , testProperty "catch: throwTo (IOSim)"
    $ forall_masking_states unit_catch_throwTo_masking_state_ST

    , testProperty "catch: throwTo async (IO)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_IO
    , testProperty "catch: throwTo async (IOSim)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_ST

    , testProperty "catch: throwTo async blocking (IO)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_mayblock_IO
    , testProperty "catch: throwTo async blocking (IOSim)"
    $ forall_masking_states unit_catch_throwTo_masking_state_async_mayblock_ST
    ]
  , testProperty "evaluate unit test" unit_evaluate_0
  , testGroup "forkIO unit tests"
    [ testProperty "1" unit_fork_1
    , testProperty "2" unit_fork_2
    ]
  , testGroup "async exception unit tests"
    [ testProperty "1"  unit_async_1
    , testProperty "2"  unit_async_2
    , testProperty "3"  unit_async_3
    , testProperty "4"  unit_async_4
    , testProperty "5"  unit_async_5
    , testProperty "6"  unit_async_6
    , testProperty "7"  unit_async_7
    , testProperty "8"  unit_async_8
    , testProperty "9"  unit_async_9
    , testProperty "10" unit_async_10
    , testProperty "11" unit_async_11
    , testProperty "12" unit_async_12
    , testProperty "13" unit_async_13
    , testProperty "14" unit_async_14
    , testProperty "15" unit_async_15
    , testProperty "16" unit_async_16
    ]
  , testGroup "STM reference semantics"
    [ testProperty "Reference vs IO"    prop_stm_referenceIO
    , testProperty "Reference vs Sim"   prop_stm_referenceSim
    ]
  , testGroup "MonadFix instances"
    [ testGroup "IOSim"
      [ testProperty "purity"     prop_mfix_purity_IOSim
      , testProperty "purity2"    prop_mfix_purity_2
      , testProperty "tightening" prop_mfix_left_shrinking_IOSim
      , testProperty "lazy"       prop_mfix_lazy
      , testProperty "recdata"    prop_mfix_recdata
      ]
    , testGroup "STM"
      [ testProperty "purity"     prop_mfix_purity_STM
      , testProperty "tightening" prop_mfix_left_shrinking_STM
      ]
    ]
  -- NOTE: Most of the tests below only work because the io-sim
  -- scheduler works the way it does.
  , testGroup "ThreadStatus"
    [ testProperty "thread status finished (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_finished
    , testProperty "thread status finished (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_finished
    , testProperty "thread status running (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_running
    , testProperty "thread status running (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_running
    , testProperty "thread status blocked (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_blocked
    , testProperty "thread status blocked (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_blocked
    , testProperty "thread status blocked delay (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_blocked_delay
    , testProperty "thread status blocked delay (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_blocked_delay
    , testProperty "thread status died (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_died
    , testProperty "thread status died (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_died
    , testProperty "thread status died_own (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_died_own
-- TODO: issue input-output-hk/io-sim#28
#if !defined(mingw32_HOST_OS)
    , testProperty "thread status died_own (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_died_own
#endif
    , testProperty "thread status yield (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_yield
    , testProperty "thread status yield (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_yield
    , testProperty "thread status mask (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_mask
-- TODO: issue input-output-hk/io-sim#28
#if !defined(mingw32_HOST_OS)
    , testProperty "thread status mask (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_mask
#endif
    , testProperty "thread status mask blocked (IOSim)"
    $ withMaxSuccess 1 $ runSimOrThrow prop_thread_status_mask_blocked
    , testProperty "thread status mask blocked (IO)"
    $ withMaxSuccess 1 $ ioProperty    prop_thread_status_mask_blocked
    ]
  ]

--
-- threadStatus
--

prop_two_threads_expect :: (MonadFork m, MonadThread m)
                        => m ()
                        -> (ThreadId m -> m ())
                        -> (ThreadStatus -> Property)
                        -> m Property
prop_two_threads_expect target main prop = do
  tid <- forkIO target
  main tid
  status <- threadStatus tid
  return $ prop status

prop_two_threads_expect_ :: (MonadFork m, MonadThread m)
                         => m ()
                         -> (ThreadStatus -> Property)
                         -> m Property
prop_two_threads_expect_ target prop =
  prop_two_threads_expect target
                          (const $ yield)
                          prop

prop_thread_status_finished :: (MonadFork m, MonadDelay m, MonadThread m)
                            => m Property
prop_thread_status_finished =
  prop_two_threads_expect_ (pure ())
                           (ThreadFinished ===)

prop_thread_status_running :: (MonadFork m, MonadDelay m, MonadThread m)
                           => m Property
prop_thread_status_running =
  prop_two_threads_expect_ (forever yield)
                           (ThreadRunning ===)

prop_thread_status_blocked :: ( MonadFork m
                              , MonadDelay m
                              , MonadThread m
                              , MonadSTM m
                              )
                           => m Property
prop_thread_status_blocked = do
  var <- newEmptyTMVarIO
  prop_two_threads_expect_
    (atomically $ takeTMVar var)
    $ \ status -> case status of
      ThreadBlocked _ -> property True
      _               ->
        counterexample (show status ++ " /= ThreadBlocked _")
                       False

prop_thread_status_blocked_delay :: (MonadFork m, MonadDelay m, MonadThread m)
                                 => m Property
prop_thread_status_blocked_delay =
  prop_two_threads_expect_
    (threadDelay 1)
    $ \ status -> case status of
      ThreadBlocked _ -> property True
      _               ->
        counterexample (show status ++ " /= ThreadBlocked _")
        False

prop_thread_status_died :: ( MonadFork m
                           , MonadThrow m
                           , MonadDelay m
                           , MonadThread m
                           )
                        => m Property
prop_thread_status_died =
  prop_two_threads_expect (forever yield)
                          (\tid -> do throwTo tid DivideByZero; yield)
                          (ThreadDied ===)

prop_thread_status_died_own :: ( MonadFork m
                               , MonadThrow m
                               , MonadDelay m
                               , MonadThread m
                               )
                            => m Property
prop_thread_status_died_own = do
  prop_two_threads_expect_ (throwIO DivideByZero)
                           (ThreadFinished ===)

prop_thread_status_yield :: ( MonadFork m
                            , MonadThrow m
                            , MonadDelay m
                            , MonadThread m
                            , MonadSTM m
                            )
                         => m Property
prop_thread_status_yield = do
  var <- newEmptyTMVarIO
  prop_two_threads_expect
    (do atomically (putTMVar var ()); forever yield)
    (const $ atomically (takeTMVar var))
    (ThreadRunning ===)

prop_thread_status_mask :: ( MonadFork m
                           , MonadThrow m
                           , MonadDelay m
                           , MonadThread m
                           , MonadSTM m
                           , MonadMask m
                           )
                        => m Property
prop_thread_status_mask = do
  var <- newEmptyTMVarIO
  prop_two_threads_expect
    (mask_ (do atomically (putTMVar var ()); yield) >> forever yield)
    (\tid -> do atomically (takeTMVar var)
                throwTo tid DivideByZero
                yield)
    (ThreadFinished ===)

prop_thread_status_mask_blocked :: ( MonadFork m
                                   , MonadThrow m
                                   , MonadThread m
                                   , MonadMask m
                                   )
                                => m Property
prop_thread_status_mask_blocked = do
  helper <- forkIO $ mask_ (forever yield)
  prop_two_threads_expect_
    (throwTo helper DivideByZero)
    $ \ status -> case status of
      ThreadBlocked _ -> property True
      _               ->
        counterexample (show status ++ " /= ThreadBlocked _")
                       False

--
-- Read/Write graph
--

prop_stm_graph_io :: TestThreadGraph -> Property
prop_stm_graph_io g =
  ioProperty $
    prop_stm_graph g

prop_stm_graph_sim :: TestThreadGraph -> Bool
prop_stm_graph_sim g =
    case runSim (prop_stm_graph g) of
       Right () -> True
       _        -> False
    -- TODO: Note that we do not use runSimStrictShutdown here to check
    -- that all other threads finished, but perhaps we should and structure
    -- the graph tests so that's the case.

prop_timers_ST :: TestMicro -> Property
prop_timers_ST (TestMicro xs) =
  let ds = map (realToFrac :: Micro -> DiffTime) xs
  in runSimOrThrow $ test_timers ds

prop_timers_IO :: [Positive Int] -> Property
prop_timers_IO = ioProperty . test_timers
               . map (microsecondsToDiffTime . (*100) . getPositive)
  where
    microsecondsToDiffTime :: Int -> DiffTime
    microsecondsToDiffTime = picosecondsToDiffTime . (* 1000000) . toInteger

--
-- Forking
--

prop_fork_order_ST :: Positive Int -> Property
prop_fork_order_ST n = runSimOrThrow $ test_fork_order n

prop_fork_order_IO :: Positive Int -> Property
prop_fork_order_IO = ioProperty . test_fork_order

prop_threadId_order_order_Sim :: Positive Int -> Property
prop_threadId_order_order_Sim n = runSimOrThrow $ test_threadId_order n

prop_wakeup_order_ST :: Property
prop_wakeup_order_ST = runSimOrThrow $ test_wakeup_order

--
-- MonadFix properties
--

-- | Purity demands that @mfix (return . f) = return (fix f)@.
--
prop_mfix_purity_m :: forall m. MonadFix m => Positive Int -> m Bool
prop_mfix_purity_m (Positive n) =
    (== fix factorial n) . ($ n) <$> mfix (return . factorial)
  where
    factorial :: (Int -> Int) -> Int -> Int
    factorial = \rec_ k -> if k <= 1 then 1 else k * rec_ (k - 1)

prop_mfix_purity_IOSim :: Positive Int -> Bool
prop_mfix_purity_IOSim a = runSimOrThrow $ prop_mfix_purity_m a

prop_mfix_purity_STM:: Positive Int -> Bool
prop_mfix_purity_STM a = runSimOrThrow $ atomically $ prop_mfix_purity_m a

prop_mfix_purity_2 :: [Positive Int] -> Bool
prop_mfix_purity_2 as =
    -- note: both 'IOSim' expressions are equivalent using 'Monad' and
    -- 'Applicative' laws only.
      runSimOrThrow (join $  mfix (return . recDelay)
                         <*> return as')
      == expected
    &&
      runSimOrThrow (mfix (return . recDelay) >>= ($ as'))
      == expected
  where
    as' :: [Int]
    as' = getPositive `map` as

    -- recursive sum using 'threadDelay'
    recDelay :: ( MonadMonotonicTime m
                , MonadDelay m
                )
             => ([Int] -> m Time)
             ->  [Int] -> m Time
    recDelay = \rec_ bs ->
                 case bs of
                  []        -> getMonotonicTime
                  (b : bs') -> threadDelay (realToFrac b)
                            >> rec_ bs'

    expected :: Time
    expected = foldl' (flip addTime)
                      (Time 0)
                      (realToFrac `map` as')


prop_mfix_left_shrinking_IOSim
    :: Int
    -> NonNegative Int
    -> Positive Int
    -> Bool
prop_mfix_left_shrinking_IOSim n (NonNegative d) (Positive i) =
   let mn :: IOSim s Int
       mn = do say ""
               threadDelay (realToFrac d)
               return n
   in
        take i
        (runSimOrThrow $
          mfix (\rec_ -> mn >>= \a -> do
                  threadDelay (realToFrac d) $> a : rec_))
      ==
        take i
        (runSimOrThrow $
          mn >>= \a ->
            (mfix (\rec_ -> do
              threadDelay (realToFrac d) $> a : rec_)))


prop_mfix_left_shrinking_STM
    :: Int
    -> Positive Int
    -> Bool
prop_mfix_left_shrinking_STM n (Positive i) =
   let mn :: STMSim s Int
       mn = do say ""
               return n
   in
        take i
        (runSimOrThrow $ atomically $
          mfix (\rec_ -> mn >>= \a -> return $ a : rec_))
      ==
        take i
        (runSimOrThrow $ atomically $
          mn >>= \a ->
            (mfix (\rec_ -> return $ a : rec_)))



-- | 'Example 8.2.1' in 'Value Recursion in Monadic Computations'
-- <https://leventerkok.github.io/papers/erkok-thesis.pdf>
--
prop_mfix_lazy :: NonEmptyList Char
               -> Bool
prop_mfix_lazy (NonEmpty env) =
         take samples
           (runSimOrThrow (withEnv (mfix . replicateHeadM)))
      == replicate samples (head env)
    where
      samples :: Int
      samples = 10

      replicateHeadM ::
                        (
#if MIN_VERSION_base(4,13,0)
                          MonadFail m,
                          MonadFail (STM m),
#endif
                          MonadSTM  m
                        )
                     => m Char
                     -> [Char] -> m [Char]
      replicateHeadM getChar_ as = do
        -- Note: 'getChar' will be executed only once! This follows from 'fixIO`
        -- semantics.
        a <- getChar_
        return (a : as)

      -- construct 'getChar' using the simulated environment
      withEnv :: (
#if MIN_VERSION_base(4,13,0)
                   MonadFail m,
#endif
                   MonadSTM  m
                 )
              => (m Char -> m a) -> m a
      withEnv k = do
        v <- newTVarIO env
        let getChar_ =
              atomically $ do
                as <- readTVar v
                case as of
                  [] -> error "withEnv: runtime error"
                  (a : as') -> writeTVar v as'
                            $> a
        k getChar_


-- | 'Example 8.2.3' in 'Value Recursion in Monadic Computations'
-- <https://leventerkok.github.io/papers/erkok-thesis.pdf>
--
prop_mfix_recdata :: Property
prop_mfix_recdata = ioProperty $ do
    expected <- experiment
    let res = runSimOrThrow experiment
    return $
      take samples res
      ==
      take samples expected
  where
    samples :: Int
    samples = 10

    experiment :: ( MonadSTM m
                  , MonadFix m
                  )
               => m [Int]
    experiment = do
      (_, y) <-
        mfix (\ ~(x, _) -> do
                y <- LazySTM.newTVarIO x
                return (1:x, y)
             )
      atomically (LazySTM.readTVar y)

--
-- Synchronous exceptions
--

unit_catch_0, unit_catch_1, unit_catch_2, unit_catch_3, unit_catch_4,
  unit_catch_5, unit_catch_6,
  unit_fork_1, unit_fork_2
  :: Property

-- unhandled top level exception
unit_catch_0 =
      runSimTraceSay example === ["before"]
 .&&. case traceResult True (runSimTrace example) of
        Left (FailureException e) -> property (maybe False (==DivideByZero) $ fromException e)
        _                         -> property False

 where
  example :: IOSim s ()
  example = do
    say "before"
    _ <- throwIO DivideByZero
    say "after"

-- normal execution of a catch frame
unit_catch_1 =
    runSimTraceSay
      (do catch (say "inner") (\(_e :: IOError) -> say "handler")
          say "after"
      )
 ===
    ["inner", "after"]


-- catching an exception thrown in a catch frame
unit_catch_2 =
    runSimTraceSay
      (do catch (do say "inner1"
                    _ <- throwIO DivideByZero
                    say "inner2")
                (\(_e :: ArithException) -> say "handler")
          say "after"
      )
 ===
    ["inner1", "handler", "after"]


-- not catching an exception of the wrong type
unit_catch_3 =
    runSimTraceSay
      (do catch (do say "inner"
                    throwIO DivideByZero)
                (\(_e :: IOError) -> say "handler")
          say "after"
      )
 ===
    ["inner"]


-- catching an exception in an outer handler
unit_catch_4 =
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(_e :: IOError) -> say "handler1"))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler2", "after"]


-- catching an exception in the inner handler
unit_catch_5 =
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(_e :: ArithException) -> say "handler1"))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler1", "after"]


-- catching an exception in the inner handler, rethrowing and catching in outer
unit_catch_6 =
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(e :: ArithException) -> do
                           say "handler1"
                           throwIO e))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler1", "handler2", "after"]


-- evaluate should catch pure errors
unit_evaluate_0 :: Property
unit_evaluate_0 =
    -- This property also fails if the @error@ is not caught by the sim monad
    -- and instead reaches the QuickCheck driver.
    property $ isLeft $ runSim $ evaluate (error "boom" :: ())


-- The sim terminates when the main thread terminates
unit_fork_1 =
      runSimTraceSay example === ["parent"]
 .&&. case traceResult True (runSimTrace example) of
        Left FailureSloppyShutdown{} -> property True
        _                            -> property False
  where
    example :: IOSim s ()
    example = do
      void $ forkIO $ say "child"
      say "parent"

-- Try works and we can pass exceptions back from threads.
-- And terminating with an exception is reported properly.
unit_fork_2 =
      runSimTraceSay example === ["parent", "user error (oh noes!)"]
 .&&. case traceResult True (runSimTrace example) of
        Left (FailureException e)
          | Just ioe <- fromException e
          , isUserError ioe
          , ioeGetErrorString ioe == "oh noes!" -> property True
        _                                       -> property False
  where
    example :: IOSim s ()
    example = do
      resVar <- newEmptyTMVarIO
      void $ forkIO $ do
        res <- try (fail "oh noes!")
        atomically (putTMVar resVar (res :: Either SomeException ()))
      say "parent"
      Left e <- atomically (takeTMVar resVar)
      say (show e)
      throwIO e


--
-- Asyncronous exceptions
--

unit_async_1, unit_async_2, unit_async_3, unit_async_4, unit_async_5,
  unit_async_6, unit_async_7, unit_async_8, unit_async_9, unit_async_10,
  unit_async_11, unit_async_12, unit_async_13, unit_async_14, unit_async_15,
  unit_async_16
  :: Property


unit_async_1 =
    runSimTraceSay
      (do mtid <- myThreadId
          say ("main " ++ show mtid)
          ctid <- forkIO $ do tid <- myThreadId
                              say ("child " ++ show tid)
          say ("parent " ++ show ctid)
          threadDelay 1
      )
 ===
   ["main ThreadId []", "parent ThreadId [1]", "child ThreadId [1]"]


unit_async_2 =
    runSimTraceSay
      (do tid <- myThreadId
          say "before"
          throwTo tid DivideByZero
          say "after"
      )
 ===
   ["before"]


unit_async_3 =
    runSimTraceSay
      (do tid <- myThreadId
          catch (do say "before"
                    throwTo tid DivideByZero
                    say "never")
                (\(_e :: ArithException) -> say "handler"))
 ===
   ["before", "handler"]


unit_async_4 =
    runSimTraceSay
      (do tid <- forkIO $ say "child"
          threadDelay 1
          -- child has already terminated when we throw the async exception
          throwTo tid DivideByZero
          say "parent done")
 ===
   ["child", "parent done"]


unit_async_5 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   say "child"
                   catch (atomically retry)
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "handler", "child done", "parent done"]


unit_async_6 =
    runSimTraceSay
      (do tid <- forkIO $ mask_ $
                   do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- while masked, do a blocking (interruptible) operation
                     catch (atomically retry)
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_7 =
    runSimTraceSay
      (do tid <- forkIO $
                   mask $ \restore -> do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- restore mask state, allowing interrupt
                     catch (restore (say "never"))
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_8 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   catch (do mask_ $ do
                               say "child"
                               threadDelay 1
                               say "child masked"
                               -- exception raised when we leave mask frame
                             say "child unmasked")
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_9 =
    runSimTraceSay
      (do tid <- forkIO $
                   mask_ $ do
                     say "child"
                     threadDelay 1
                     fail "oh noes!"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          -- throwTo blocks but then unblocks because the child dies
          say "parent done")
 ===
   ["child", "parent done"]


unit_async_10 =
    runSimTraceSay
      (do tid1 <- forkIO $ do
                    mask_ $ do
                      threadDelay 1
                      say "child 1"
                      yield
                      say "child 1 running"
                    say "never 1"
          tid2 <- forkIO $ do
                      threadDelay 1
                      say "child 2"
                      -- this one blocks, since child 1 is running with
                      -- async exceptions masked
                      throwTo tid1 DivideByZero
                      say "never 2"
          threadDelay 1
          yield
          -- this one does not block, child 2 does not have exceptions
          -- masked (and it is blocked in an interruptible throwTo)
          throwTo tid2 DivideByZero
          threadDelay 1
          say "parent done"
          )
 ===
   ["child 1", "child 2", "child 1 running", "parent done"]


unit_async_11 =
    runSimTraceSay
      (do tid1 <- forkIO $ do
                    mask_ $ do
                      threadDelay 1
                      say "child 1"
                      yield
                      say "child 1 running"
                    say "never 1"
          tid2 <- forkIO $
                    -- Same as unit_async_10 but we run masked here
                    -- this is subtle: when the main thread throws the
                    -- exception it raises the exception here even though
                    -- it is masked because this thread is blocked in the
                    -- throwTo and so is interruptible.
                    mask_ $ do
                      threadDelay 1
                      say "child 2"
                      throwTo tid1 DivideByZero
                      say "never 2"
          threadDelay 1
          yield
          -- this one does not block, even though child 2 has exceptions
          -- masked, since it is blocked in an interruptible throwTo
          throwTo tid2 DivideByZero
          threadDelay 1
          say "parent done"
          )
 ===
   ["child 1", "child 2", "child 1 running", "parent done"]


unit_async_12 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   uninterruptibleMask_ $ do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- while masked, do a blocking (interruptible) operation
                     catch (threadDelay 1)
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
                   say "never"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "child done", "parent done"]


unit_async_13 =
    case runSim
           (uninterruptibleMask_ $ do
              tid <- forkIO $ atomically retry
              throwTo tid DivideByZero)
       of Left FailureDeadlock {} -> property True
          _                       -> property False


unit_async_14 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   uninterruptibleMask_ $ do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- while masked do a blocking operation, but this is
                     -- an uninterruptible mask so nothing happens
                     catch (threadDelay 1)
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
                   say "never"
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "child done", "parent done"]


unit_async_15 =
    runSimTraceSay
      (do tid <- forkIO $
                   uninterruptibleMask $ \restore -> do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- restore mask state, allowing interrupt
                     catch (restore (say "never"))
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_16 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   catch (do uninterruptibleMask_ $ do
                               say "child"
                               threadDelay 1
                               say "child masked"
                               -- exception raised when we leave mask frame
                             say "child unmasked")
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


--
-- Tests vs STM operational semantics
--

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the real IO STM implementation.
--
prop_stm_referenceIO :: SomeTerm -> Property
prop_stm_referenceIO t =
    ioProperty (prop_stm_referenceM t)

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the IO simulator's STM implementation.
--
prop_stm_referenceSim :: SomeTerm -> Property
prop_stm_referenceSim t =
    runSimOrThrow (prop_stm_referenceM t)

prop_timeout_no_deadlock_Sim :: Bool
prop_timeout_no_deadlock_Sim = runSimOrThrow prop_timeout_no_deadlockM

prop_timeout_no_deadlock_IO :: Property
prop_timeout_no_deadlock_IO = ioProperty prop_timeout_no_deadlockM


--
-- MonadMask properties
--

unit_set_masking_state_IO :: MaskingState -> Property
unit_set_masking_state_IO =
    ioProperty . prop_set_masking_state

unit_set_masking_state_ST :: MaskingState -> Property
unit_set_masking_state_ST ms =
    runSimOrThrow (prop_set_masking_state ms)

unit_unmask_IO :: MaskingState -> MaskingState -> Property
unit_unmask_IO ms ms' = ioProperty $ prop_unmask ms ms'

unit_unmask_ST :: MaskingState -> MaskingState -> Property
unit_unmask_ST ms ms' = runSimOrThrow $ prop_unmask ms ms'

unit_fork_masking_state_IO :: MaskingState -> Property
unit_fork_masking_state_IO =
    ioProperty . prop_fork_masking_state

unit_fork_masking_state_ST :: MaskingState -> Property
unit_fork_masking_state_ST ms =
    runSimOrThrow (prop_fork_masking_state ms)

unit_fork_unmask_IO :: MaskingState -> MaskingState -> Property
unit_fork_unmask_IO ms ms' = ioProperty $ prop_fork_unmask ms ms'

unit_fork_unmask_ST :: MaskingState -> MaskingState -> Property
unit_fork_unmask_ST ms ms' = runSimOrThrow $ prop_fork_unmask ms ms'

unit_catch_throwIO_masking_state_IO :: MaskingState -> Property
unit_catch_throwIO_masking_state_IO ms =
    ioProperty $ prop_catch_throwIO_masking_state ms

unit_catch_throwIO_masking_state_ST :: MaskingState -> Property
unit_catch_throwIO_masking_state_ST ms =
    runSimOrThrow (prop_catch_throwIO_masking_state ms)

unit_catch_throwTo_masking_state_IO :: MaskingState -> Property
unit_catch_throwTo_masking_state_IO =
    ioProperty . prop_catch_throwTo_masking_state

unit_catch_throwTo_masking_state_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_ST ms =
    runSimOrThrow $ prop_catch_throwTo_masking_state ms

unit_catch_throwTo_masking_state_async_IO :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_IO =
    ioProperty . prop_catch_throwTo_masking_state_async

unit_catch_throwTo_masking_state_async_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_ST ms =
    runSimOrThrow (prop_catch_throwTo_masking_state_async ms)

unit_catch_throwTo_masking_state_async_mayblock_IO :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_mayblock_IO =
    ioProperty . prop_catch_throwTo_masking_state_async_mayblock

unit_catch_throwTo_masking_state_async_mayblock_ST :: MaskingState -> Property
unit_catch_throwTo_masking_state_async_mayblock_ST ms =
    runSimOrThrow (prop_catch_throwTo_masking_state_async_mayblock ms)
