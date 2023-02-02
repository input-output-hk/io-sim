{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Control.Monad.Utils where

import           Data.Array
import           Data.Fixed (Fixed (..), Micro)
import           Data.Function (on)
import           Data.Graph
import           Data.List (sortBy)

import           Control.Monad

import           Control.Monad.Class.MonadFork
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           Test.Control.Monad.STM

import           Test.QuickCheck

--
-- Read/Write graph
--

prop_stm_graph :: (MonadFork m, MonadSTM m) => TestThreadGraph -> m ()
prop_stm_graph (TestThreadGraph g) = do
    vars <- listArray (bounds g) <$>
            sequence [ newTVarIO False | _ <- vertices g ]
    forM_ (vertices g) $ \v ->
      void $ forkIO $ do
        -- read all the inputs and wait for them to become true
        -- then write to all the outputs
        let incomming = g' ! v
            outgoing  = g  ! v
        atomically $ do
          sequence_ [ readTVar  (vars ! var) >>= check | var <- incomming ]
          sequence_ [ writeTVar (vars ! var) True      | var <- outgoing  ]

    let -- the vertices with outgoing but no incoming edges
        inputs  = [ v
                  | v <- vertices g
                  , not (null (g  ! v))
                  ,      null (g' ! v) ]
        -- the vertices with incoming but no outgoing edges
        outputs = [ v
                  | v <- vertices g
                  , not (null (g' ! v))
                  ,      null (g  ! v) ]

    -- write to the inputs and wait for the outputs
    void $ forkIO $ atomically $ sequence_ [ writeTVar (vars ! var) True | var <- inputs  ]
    atomically $ sequence_ [ readTVar (vars ! var) >>= check | var <- outputs ]
  where
    g' = transposeG g -- for incoming edges

newtype TestThreadGraph = TestThreadGraph Graph
  deriving Show

instance Arbitrary TestThreadGraph where
  arbitrary =
    sized $ \sz ->
    TestThreadGraph <$> arbitraryAcyclicGraph
                          (choose (2, 8 `min` (sz `div` 3)))
                          (choose (1, 8 `min` (sz `div` 3)))
                          0.3

arbitraryAcyclicGraph :: Gen Int -> Gen Int -> Float -> Gen Graph
arbitraryAcyclicGraph genNRanks genNPerRank edgeChance = do
    nranks    <- genNRanks
    rankSizes <- replicateM nranks genNPerRank
    let rankStarts = scanl (+) 0 rankSizes
        rankRanges = drop 1 (zip rankStarts (tail rankStarts))
        totalRange = sum rankSizes
    rankEdges <- mapM (uncurry genRank) rankRanges
    return $ buildG (0, totalRange-1) (concat rankEdges)
  where
    genRank :: Vertex -> Vertex -> Gen [Edge]
    genRank rankStart rankEnd =
      filterM (const (pick edgeChance))
        [ (i,j)
        | i <- [0..rankStart-1]
        , j <- [rankStart..rankEnd-1]
        ]

    pick :: Float -> Gen Bool
    pick chance = (< chance) <$> choose (0,1)


--
-- Timers
--

newtype TestMicro = TestMicro [Micro]
  deriving Show

-- |
-- Arbitrary non negative micro numbers with a high probability of
-- repetitions.
instance Arbitrary TestMicro where
  arbitrary = sized $ \n -> TestMicro <$> genN n []
    where
      genN :: Int -> [Micro] -> Gen [Micro]
      genN 0 rs = return rs
      genN n [] = do
        r <- genMicro
        genN (n - 1) [r]
      genN n rs = do
        r <- frequency
          [ (2, elements rs)
          , (1, genMicro)
          ]
        genN (n - 1) (r : rs)

      genMicro :: Gen Micro
      genMicro = MkFixed <$> arbitrary

  shrink (TestMicro rs) = [ TestMicro rs' | rs' <- shrinkList (const []) rs ]

test_timers :: forall m.
               ( MonadFork m
               , MonadSTM m
               , MonadTimer m
               )
            => [DiffTime]
            -> m Property
test_timers xs =
    label (lbl xs) . isValid <$> withProbe experiment
  where
    countUnique :: Eq a => [a] -> Int
    countUnique [] = 0
    countUnique (a:as) =
      let as' = filter (== a) as
      in 1 + countUnique as'

    lbl :: Eq a => [a] -> String
    lbl as =
      let p = (if null as then 0 else (100 * countUnique as) `div` length as) `mod` 10 * 10
      in show p ++ "% unique"

    experiment :: Probe m (DiffTime, Int) -> m ()
    experiment p = do
      tvars <- forM (zip xs [0..]) $ \(t, idx) -> do
        v <- newTVarIO False
        void $ forkIO $ threadDelay t >> do
          probeOutput p (t, idx)
          atomically $ writeTVar v True
        return v

      -- wait for all tvars
      forM_ tvars $ \v -> atomically (readTVar v >>= check)

    isValid :: [(DiffTime, Int)] -> Property
    isValid tr =
         -- all timers should fire
         (length tr === length xs)
         -- timers should fire in the right order
      .&&. (sortBy (on sortFn fst) tr === tr)

    -- timers with negative timeout never fired, so we treat them as they would
    -- all fired at once at @-∞@.  This is to say that the following function is
    -- a well defined partial order.
    sortFn :: DiffTime -> DiffTime -> Ordering
    sortFn a b | a >= 0 && b >= 0 = a `compare` b
               | a  < 0 && b  < 0 = EQ
               | otherwise = a `compare` b

--
-- Forking
--

test_fork_order :: forall m.
                   ( MonadFork m
                   , MonadSTM m
                   , MonadTimer m
                   )
                => Positive Int
                -> m Property
test_fork_order = \(Positive n) -> isValid n <$> withProbe (experiment n)
  where
    experiment :: Int -> Probe m Int -> m ()
    experiment 0 _ = return ()
    experiment n p = do
      v <- newTVarIO False

      void $ forkIO $ do
        probeOutput p n
        atomically $ writeTVar v True
      experiment (n - 1) p

      -- wait for the spawned thread to finish
      atomically $ readTVar v >>= check

    isValid :: Int -> [Int] -> Property
    isValid n tr = tr === [n,n-1..1]

test_threadId_order :: forall m.
                   ( MonadFork m
                   , MonadSTM m
                   , MonadTimer m
                   )
                => Positive Int
                -> m Property
test_threadId_order = \(Positive n) -> do
    isValid n <$> (forM [1..n] (const experiment))
  where
    experiment :: m (ThreadId m)
    experiment = do
      v <- newTVarIO False

      tid <- forkIO $ atomically $ writeTVar v True

      -- wait for the spawned thread to finish
      atomically $ readTVar v >>= check
      return tid

    isValid :: Int -> [ThreadId m] -> Property
    isValid n tr = map show tr === map (("ThreadId " ++ ) . show . (:[])) [1..n]

-- This property is not actually deterministic in IO. Uncomment the following
-- and try it! Arguably therefore, this property does not need to be true for
-- the Sim either. Perhaps we should introduce random scheduling and abandon
-- this property. In the meantime it's a helpful sanity check.

--prop_wakeup_order_IO = ioProperty test_wakeup_order

test_wakeup_order :: ( MonadFork m
                     , MonadSTM m
                     , MonadTimer m
                     )
                => m Property
test_wakeup_order = do
    v          <- newTVarIO False
    wakupOrder <-
      withProbe $ \p -> do
        sequence_
          [ do _ <- forkIO $ do
                 atomically $ do
                   x <- readTVar v
                   check x
                 probeOutput p (n :: Int)
               threadDelay 0.1
          | n <- [0..9] ]
        atomically $ writeTVar v True
        threadDelay 0.1
    return (wakupOrder === [0..9]) --FIFO order

--
-- Probe mini-abstraction
--

-- | Where returning results directly is not convenient, we can build up
-- a trace of events we want to observe, and can do probe output from
-- multiple threads.
--
type Probe m x = StrictTVar m [x]

withProbe :: MonadSTM m => (Probe m x -> m ()) -> m [x]
withProbe action = do
    probe <- newTVarIO []
    action probe
    reverse <$> atomically (readTVar probe)

probeOutput :: MonadSTM m => Probe m x -> x -> m ()
probeOutput probe x = atomically (modifyTVar probe (x:))

--
-- Tests vs STM operational semantics
--

--TODO: would be nice to also have stronger tests here:
-- * compare all the tvar values in the heap
-- * compare the read and write sets

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of any 'MonadSTM' STM implementation.
--
prop_stm_referenceM :: (MonadSTM m, MonadThrow (STM m), MonadCatch (STM m), MonadCatch m)
                    => SomeTerm -> m Property
prop_stm_referenceM (SomeTerm _tyrep t) = do
    let (r1, _heap) = evalAtomically t
    r2 <- execAtomically t
    return (r1 === r2)

-- | Check that 'timeout' does not deadlock when executed with asynchronous
-- exceptions uninterruptibly masked.
--
prop_timeout_no_deadlockM :: forall m. ( MonadFork m, MonadSTM m, MonadTimer m, MonadMask m )
                          => m Bool
prop_timeout_no_deadlockM = do
    v <- registerDelay' 0.01
    r <- uninterruptibleMask_ $ timeout 0.02 $ do
      atomically $ do
        readTVar v >>= check
        return True
    case r of
      Nothing -> return False
      Just b  -> return b
  where
    -- Like 'registerDelay', but does not require threaded RTS in the @m ~ IO@
    -- case.
    registerDelay' :: DiffTime -> m (StrictTVar m Bool)
    registerDelay' delta = do
      v <- newTVarIO False
      _ <- forkIO $ do
             threadDelay delta
             atomically (writeTVar v True)
      return v

--
-- MonadMask properties
--

setMaskingState_ :: MonadMask m => MaskingState -> m a -> m a
setMaskingState_ Unmasked              = id
setMaskingState_ MaskedInterruptible   = mask_
setMaskingState_ MaskedUninterruptible = uninterruptibleMask_

setMaskingState :: MonadMask m => MaskingState
                -> ((forall x. m x -> m x) -> m a) -> m a
setMaskingState Unmasked              = \f -> f id
setMaskingState MaskedInterruptible   = mask
setMaskingState MaskedUninterruptible = uninterruptibleMask

maxMS :: MaskingState -> MaskingState -> MaskingState
maxMS MaskedUninterruptible _                     = MaskedUninterruptible
maxMS _                     MaskedUninterruptible = MaskedUninterruptible
maxMS MaskedInterruptible   _                     = MaskedInterruptible
maxMS _                     MaskedInterruptible   = MaskedInterruptible
maxMS Unmasked              Unmasked              = Unmasked

-- | Check that setting masking state is effective.
--
prop_set_masking_state :: MonadMaskingState m
                       => MaskingState
                       -> m Property
prop_set_masking_state ms =
    setMaskingState_ ms $ do
      ms' <- getMaskingState
      return (ms === ms')

-- | Check that 'unmask' restores the masking state.
--
prop_unmask :: MonadMaskingState m
            => MaskingState
            -> MaskingState
            -> m Property
prop_unmask ms ms' =
    setMaskingState_ ms $
      setMaskingState ms' $ \unmask -> do
        ms'' <- unmask getMaskingState
        return (ms'' === ms)

-- | Check that masking state is inherited by a forked thread.
--
prop_fork_masking_state :: ( MonadMaskingState m
                           , MonadFork m
                           , MonadSTM m
                           )
                        => MaskingState -> m Property
prop_fork_masking_state ms = setMaskingState_ ms $ do
    var <- newEmptyTMVarIO
    _ <- forkIO $ getMaskingState >>= atomically . putTMVar var
    ms' <- atomically $ takeTMVar var
    return $ ms === ms'

-- | Check that 'unmask' restores the masking state in a forked thread.
--
-- Note: unlike 'prop_unmask', 'forkIOWithUnmask's 'unmask' function will
-- restore 'Unmasked' state, not the encosing masking state.
--
prop_fork_unmask :: ( MonadMaskingState m
                    , MonadFork m
                    , MonadSTM m
                    )
                 => MaskingState
                 -> MaskingState
                 -> m Property
prop_fork_unmask ms ms' =
    setMaskingState_ ms $
      setMaskingState_ ms' $ do
        var <- newEmptyTMVarIO
        _ <- forkIOWithUnmask $ \unmask -> unmask getMaskingState
                                       >>= atomically . putTMVar var
        ms'' <- atomically $ takeTMVar var
        return $ Unmasked === ms''

-- | A unit test which checks the masking state in the context of a catch
-- handler.
--
prop_catch_throwIO_masking_state :: forall m. MonadMaskingState m
                                 => MaskingState -> m Property
prop_catch_throwIO_masking_state ms =
    setMaskingState_ ms $ do
      throwIO (userError "error")
      `catch` \(_ :: IOError) -> do
        ms' <- getMaskingState
        return $ ms' === MaskedInterruptible `maxMS` ms

-- | Like 'prop_catch_masking_state' but using 'throwTo'.
--
prop_catch_throwTo_masking_state :: forall m.
                                    ( MonadMaskingState m
                                    , MonadFork m
                                    )
                                 => MaskingState -> m Property
prop_catch_throwTo_masking_state ms =
    setMaskingState_ ms $ do
      tid <- myThreadId
      (throwTo tid (userError "error") >> error "impossible")
      `catch` \(_ :: IOError) -> do
        ms' <- getMaskingState
        return $ ms' === MaskedInterruptible `maxMS` ms

-- | Like 'prop_catch_throwTo_masking_state' but using 'throwTo' to a different
-- thread which is in a non-blocking mode.
--
prop_catch_throwTo_masking_state_async :: forall m.
                                          ( MonadMaskingState m
                                          , MonadFork  m
                                          , MonadSTM   m
                                          , MonadDelay m
                                          )
                                       => MaskingState -> m Property
prop_catch_throwTo_masking_state_async ms = do
    sgnl <- newEmptyTMVarIO
    var <- newEmptyTMVarIO
    tid <- forkIO $
      setMaskingState ms $ \unmask ->
        (do atomically $ putTMVar sgnl ()
            unmask (threadDelay 1)
        )
        `catch` \(_ :: IOError) -> do
          ms' <- getMaskingState
          atomically $ putTMVar var (ms' === ms `maxMS` MaskedInterruptible)
    -- wait until the catch handler is installed
    atomically $ takeTMVar sgnl
    -- the forked thread is interruptibly blocked on `threadDelay`,
    -- `throwTo` will not block
    throwTo tid (userError "error")
    atomically $ takeTMVar var

-- | Like 'prop_catch_throwTo_masking_state_async' but 'throwTo' will block if
-- masking state is set to 'MaskedUninterruptible'.  This makes sure that the
-- 'willBlock' branch of 'ThrowTo' in 'schedule' is covered.
--
prop_catch_throwTo_masking_state_async_mayblock :: forall m.
                                                ( MonadMaskingState m
                                                , MonadFork  m
                                                , MonadSTM   m
                                                , MonadDelay m
                                                )
                                             => MaskingState -> m Property
prop_catch_throwTo_masking_state_async_mayblock ms = do
    sgnl <- newEmptyTMVarIO
    var <- newEmptyTMVarIO
    tid <- forkIO $
      setMaskingState ms $ \unmask ->
        (do atomically $ putTMVar sgnl ()
            -- if 'ms' is 'MaskedUninterruptible' then the following
            -- 'threadDelay' will block.
            threadDelay 0.1
            -- make sure that even in 'MaskedUninterruptible' the thread
            -- unblocks so async exceptions can be delivered.
            unmask (threadDelay 1)
        )
        `catch` \(_ :: IOError) -> do
          ms' <- getMaskingState
          atomically $ putTMVar var (ms' === ms `maxMS` MaskedInterruptible)
    -- wait until the catch handler is installed
    atomically $ takeTMVar sgnl
    threadDelay 0.05
    -- we know the forked thread is interruptibly blocked on `threadDelay`,
    -- `throwTo` will not be blocked.
    throwTo tid (userError "error")
    atomically $ takeTMVar var

--
-- MonadMask properties
--

forall_masking_states :: (MaskingState -> Property)
                      -> Property
forall_masking_states prop =
    -- make sure that the property is executed once!
    withMaxSuccess 1 $
    foldr (\ms p -> counterexample (show ms) (prop ms) .&&. p)
          (property True)
          [Unmasked, MaskedInterruptible, MaskedUninterruptible]

--
-- Utils
--

runSimTraceSay :: (forall s. IOSim s a) -> [String]
runSimTraceSay action = selectTraceSay (runSimTrace action)

selectTraceSay :: SimTrace a -> [String]
selectTraceSay (SimTrace _ _ _ (EventSay msg) trace)      = msg : selectTraceSay trace
selectTraceSay (SimTrace _ _ _ _              trace)      = selectTraceSay trace
selectTraceSay (SimPORTrace _ _ _ _ (EventSay msg) trace) = msg : selectTraceSay trace
selectTraceSay (SimPORTrace _ _ _ _ _              trace) = selectTraceSay trace
selectTraceSay  _                                         = []

