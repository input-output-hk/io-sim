{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | 'io-sim' implementation of 'TQueue', 'TBQueue' and 'MVar'.
--
-- Unlike the default implementation available in 'io-classes' the 'TQueue' and
-- 'TBQueue' are using a single 'TVar', which simplifies the implementation of
-- 'traceTQueue' and 'traceTBQueue' methods.
--
module Control.Monad.IOSim.STM where

import Control.Exception (SomeAsyncException (..))

import Control.Concurrent.Class.MonadSTM.TVar
import Control.Monad.Class.MonadSTM (MonadInspectSTM (..), MonadLabelledSTM,
           MonadSTM (..), MonadTraceSTM, TraceValue (..))
import Control.Monad.Class.MonadThrow

import Numeric.Natural (Natural)

import Data.Deque.Strict (Deque)
import Data.Deque.Strict qualified as Deque

--
-- Default TQueue implementation in terms of a 'TVar' (used by sim)
--

newtype TQueueDefault m a = TQueue (TVar m ([a], [a]))

labelTQueueDefault
  :: MonadLabelledSTM m
  => TQueueDefault m a -> String -> STM m ()
labelTQueueDefault (TQueue queue) label = labelTVar queue label

traceTQueueDefault
  :: MonadTraceSTM m
  => proxy m
  -> TQueueDefault m a
  -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
  -> STM m ()
traceTQueueDefault p (TQueue queue) f =
    traceTVar p queue
              (\mas as -> f (g <$> mas) (g as))
  where
    g (xs, ys) = xs ++ reverse ys

newTQueueDefault :: MonadSTM m => STM m (TQueueDefault m a)
newTQueueDefault = TQueue <$> newTVar ([], [])

writeTQueueDefault :: MonadSTM m => TQueueDefault m a -> a -> STM m ()
writeTQueueDefault (TQueue queue) a = do
    (xs, ys) <- readTVar queue
    writeTVar queue $! (xs, a : ys)

readTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
readTQueueDefault queue = maybe retry return =<< tryReadTQueueDefault queue

tryReadTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryReadTQueueDefault (TQueue queue) = do
  (xs, ys) <- readTVar queue
  case xs of
    (x:xs') -> do
      writeTVar queue $! (xs', ys)
      return (Just x)
    [] ->
      case reverse ys of
        []     -> return Nothing
        (z:zs) -> do
          writeTVar queue $! (zs, [])
          return (Just z)

isEmptyTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m Bool
isEmptyTQueueDefault (TQueue queue) = do
  (xs, ys) <- readTVar queue
  return $ case xs of
    _:_ -> False
    []  -> case ys of
             [] -> True
             _  -> False

peekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
peekTQueueDefault (TQueue queue) = do
    (xs, _) <- readTVar queue
    case xs of
      x :_ -> return x
      []   -> retry

tryPeekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryPeekTQueueDefault (TQueue queue) = do
    (xs, _) <- readTVar queue
    return $ case xs of
      x :_ -> Just x
      []   -> Nothing

flushTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m [a]
flushTQueueDefault (TQueue queue) = do
  (xs, ys) <- readTVar queue
  writeTVar queue ([], [])
  pure (xs <> reverse ys)

unGetTQueueDefault :: MonadSTM m => TQueueDefault m a -> a -> STM m ()
unGetTQueueDefault (TQueue queue) a = do
    (xs, ys) <- readTVar queue
    writeTVar queue (a : xs, ys)

--
-- Default TBQueue implementation in terms of 'Seq' (used by sim)
--

data TBQueueDefault m a = TBQueue
  !(TVar m ([a], Natural, [a], Natural))
  !Natural

labelTBQueueDefault
  :: MonadLabelledSTM m
  => TBQueueDefault m a -> String -> STM m ()
labelTBQueueDefault (TBQueue queue _size) label = labelTVar queue label

traceTBQueueDefault
  :: MonadTraceSTM m
  => proxy m
  -> TBQueueDefault m a
  -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
  -> STM m ()
traceTBQueueDefault p (TBQueue queue _size) f =
    traceTVar p queue (\mas as -> f (g <$> mas) (g as))
  where
    g (xs, _, ys, _) = xs ++ reverse ys


newTBQueueDefault :: MonadSTM m => Natural -> STM m (TBQueueDefault m a)
newTBQueueDefault size | size >= fromIntegral (maxBound :: Int)
                       = error "newTBQueueDefault: size larger than Int"
newTBQueueDefault size =
  flip TBQueue size <$> (newTVar $! ([], 0, [], size))

readTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
readTBQueueDefault queue = maybe retry return =<< tryReadTBQueueDefault queue

tryReadTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryReadTBQueueDefault (TBQueue queue _size) = do
  (xs, r, ys, w) <- readTVar queue
  let !r' = r + 1
  case xs of
    (x:xs') -> do
      writeTVar queue $! (xs', r', ys, w)
      return (Just x)
    [] ->
      case reverse ys of
        [] -> return Nothing

        -- NB. lazy: we want the transaction to be
        -- short, otherwise it will conflict
        (z:zs) -> do
           writeTVar queue $! (zs, r', [], w)
           return (Just z)

peekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
peekTBQueueDefault queue = maybe retry return =<< tryPeekTBQueueDefault queue

tryPeekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryPeekTBQueueDefault (TBQueue queue _size) = do
    (xs, _, _, _) <- readTVar queue
    return $ case xs of
      (x:_) -> Just x
      _     -> Nothing

writeTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> STM m ()
writeTBQueueDefault (TBQueue queue _size) a = do
  (xs, r, ys, w) <- readTVar queue
  if (w > 0)
    then do let !w' = w - 1
            writeTVar queue $! (xs, r, a:ys, w')
    else do
          if (r > 0)
            then let !w' = r - 1 in
                 writeTVar queue (xs, 0, a:ys, w')
            else retry

isEmptyTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isEmptyTBQueueDefault (TBQueue queue _size) = do
  (xs, _, ys, _) <- readTVar queue
  case xs of
    _:_ -> return False
    []  -> case ys of
             [] -> return True
             _  -> return False

isFullTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isFullTBQueueDefault (TBQueue queue _size) = do
  (_, r, _, w) <- readTVar queue
  return $
    if (w > 0)
    then False
    else if (r > 0)
         then False
         else True

lengthTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Natural
lengthTBQueueDefault (TBQueue queue size) = do
  (_, r, _, w) <- readTVar queue
  return $! size - r - w

flushTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m [a]
flushTBQueueDefault (TBQueue queue size) = do
  (xs, _, ys, _) <- readTVar queue
  if null xs && null ys
    then return []
    else do
      writeTVar queue $! ([], 0, [], size)
      return (xs ++ reverse ys)

unGetTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> STM m ()
unGetTBQueueDefault (TBQueue queue _size) a = do
  (xs, r, ys, w) <- readTVar queue
  if (r > 0)
     then do writeTVar queue (a : xs, r - 1, ys, w)
     else do
          if (w > 0)
             then writeTVar queue (a : xs, r, ys, w - 1)
             else retry


--
-- Default MVar implementation in terms of STM (used by sim)
--

-- | A default 'MonadMVar' implementation is based on `TVar`'s.  An @MVar@
-- guarantees fairness.
--
-- /Implementation details:/
--
-- @STM@ does not guarantee fairness, instead it provides compositionally.
-- Fairness of 'putMVarDefault' and 'takeMVarDefault' is provided by tracking
-- queue of blocked operation in the 'MVarState', e.g.  when a 'putMVarDefault'
-- is scheduled on a full 'MVar', the request is put on to the back of the queue
-- together with a wakeup var.  When 'takeMVarDefault' executes, it returns the
-- value and is using the first element of the queue to set the new value of
-- the 'MVar' and signals next `putMVarDefault` operation to unblock.  This has
-- an effect as if all the racing `putMVarDefault` calls where executed in
-- turns.
--
-- Note that 'readMVar' has interesting semantics: it is guaranteed to read
-- the next value put using 'putMVar', and all readers will wake up, not just
-- the first. To support this, the implementation uses two queues in the empty
-- MVar case: one for threads blocked on 'takeMVar', and one for threads
-- blocked on 'readMVar'. The 'putMVar' has to wake up all readers and the
-- first \"taker\" (if any).
--
newtype MVarDefault m a = MVar (TVar m (MVarState m a))

data MVarState m a = MVarEmpty   !(Deque (TVar m (Maybe a))) -- blocked on take
                                 !(Deque (TVar m (Maybe a))) -- blocked on read
                   | MVarFull  a !(Deque (a, TVar m Bool))   -- blocked on put


newEmptyMVarDefault :: MonadSTM m => m (MVarDefault m a)
newEmptyMVarDefault = MVar <$> newTVarIO (MVarEmpty mempty mempty)

labelMVarDefault
  :: MonadLabelledSTM m
  => MVarDefault m a -> String -> m ()
labelMVarDefault (MVar tvar) = atomically . labelTVar tvar

newMVarDefault :: MonadSTM m => a -> m (MVarDefault m a)
newMVarDefault a = MVar <$> newTVarIO (MVarFull a mempty)


putMVarDefault :: ( MonadMask  m
                  , MonadLabelledSTM m
                  , forall x tvar. tvar ~ TVar m x => Eq tvar
                  )
               => MVarDefault m a -> a -> m ()
putMVarDefault (MVar tv) x = mask_ $ do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- It's full, add ourselves to the end of the 'put' blocked queue.
        MVarFull x' putq -> do
          putvar <- newTVar False
          labelTVar putvar "internal-putvar"
          writeTVar tv (MVarFull x' (Deque.snoc (x, putvar) putq))
          return (Just putvar)

        -- The MVar is empty. Wake up any threads blocked in readMVar.
        -- If there's at least one thread blocked in takeMVar, we wake up the
        -- first, leaving the MVar empty. Otherwise the MVar becomes full.
        MVarEmpty takeq readq -> do
          mapM_ (\readvar -> writeTVar readvar (Just x)) readq

          case Deque.uncons takeq of
            Nothing ->
              writeTVar tv (MVarFull x mempty)

            Just (takevar, takeq') -> do
              writeTVar takevar (Just x)
              writeTVar tv (MVarEmpty takeq' mempty)

          return Nothing

    case res of
      -- we have to block on our own putvar until we can complete the put
      Just putvar ->
        atomically (readTVar putvar >>= check)
        `catch` \e@SomeAsyncException {} -> do
          atomically $ do
            s <- readTVar tv
            case s of
              MVarFull x' putq -> do
                -- async exception was thrown while we were blocked on putvar;
                -- we need to remove it from the queue, otherwise we will have
                -- a space leak.
                let putq' = Deque.filter ((/= putvar) . snd) putq
                writeTVar tv (MVarFull x' putq')

              -- This case is unlikely but possible if another thread ran
              -- first and modified the mvar. This situation is fine as far as
              -- space leaks are concerned because it means our wait var is no
              -- longer in the wait queue.
              MVarEmpty {} -> return ()
          throwIO e

      -- we managed to do the put synchronously
      Nothing -> return ()


tryPutMVarDefault :: MonadSTM m
                  => MVarDefault m a -> a -> m Bool
tryPutMVarDefault (MVar tv) x =
    atomically $ do
      s <- readTVar tv
      case s of
        MVarFull {} -> return False

        -- The MVar is empty. Wake up any threads blocked in readMVar.
        -- If there's at least one thread blocked in takeMVar, we wake up the
        -- first, leaving the MVar empty. Otherwise the MVar becomes full.
        MVarEmpty takeq readq -> do

          mapM_ (\readvar -> writeTVar readvar (Just x)) readq

          case Deque.uncons takeq of
            Nothing ->
              writeTVar tv (MVarFull x mempty)

            Just (takevar, takeq') -> do
              writeTVar takevar (Just x)
              writeTVar tv (MVarEmpty takeq' mempty)

          return True


takeMVarDefault :: ( MonadMask m
                   , MonadLabelledSTM m
                   , forall x tvar. tvar ~ TVar m x => Eq tvar
                   )
                => MVarDefault m a
                -> m a
takeMVarDefault (MVar tv) = mask_ $ do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- It's empty, add ourselves to the end of the 'take' blocked queue.
        MVarEmpty takeq readq -> do
          takevar <- newTVar Nothing
          labelTVar takevar "internal-takevar"
          writeTVar tv (MVarEmpty (Deque.snoc takevar takeq) readq)
          return (Left takevar)

        -- It's full. If there's at least one thread blocked in putMVar, wake
        -- up the first one leaving the MVar full with the new put value.
        -- Otherwise the MVar becomes empty.
        MVarFull x putq ->
          case Deque.uncons putq of
            Nothing -> do
              writeTVar tv (MVarEmpty mempty mempty)
              return (Right x)

            Just ((x', putvar), putq') -> do
              writeTVar putvar True
              writeTVar tv (MVarFull x' putq')
              return (Right x)

    case res of
      -- we have to block on our own takevar until we can complete the read
      Left takevar ->
        atomically (readTVar takevar >>= maybe retry return)
        `catch` \e@SomeAsyncException {} -> do
          atomically $ do
            s <- readTVar tv
            case s of
              MVarEmpty takeq readq -> do
                -- async exception was thrown while were were blocked on
                -- takevar; we need to remove it from 'takeq', otherwise we
                -- will have a space leak.
                let takeq' = Deque.filter (/= takevar) takeq
                takevalue <- readTVar takevar
                case takevalue of
                  Nothing ->
                    writeTVar tv (MVarEmpty takeq' readq)
                  -- we were given a value before we could read it. Relay it to any
                  -- new reading threads and possible the next take thread.
                  Just x -> do
                    -- notify readers
                    mapM_ (\readvar -> writeTVar readvar (Just x)) readq

                    -- notify first `takeMVar` thread
                    case Deque.uncons takeq' of
                      Nothing ->
                        writeTVar tv (MVarFull x mempty)

                      Just (takevar', takeq'') -> do
                        writeTVar takevar' (Just x)
                        writeTVar tv (MVarEmpty takeq'' mempty)

              -- This case is unlikely but possible if another thread ran
              -- first and modified the mvar. This situation is fine as far as
              -- space leaks are concerned because it means our wait var is no
              -- longer in the wait queue.
              MVarFull {} -> return ()
          throwIO e

      -- we managed to do the take synchronously
      Right x -> return x


tryTakeMVarDefault :: MonadSTM m
                   => MVarDefault m a
                   -> m (Maybe a)
tryTakeMVarDefault (MVar tv) = do
    atomically $ do
      s <- readTVar tv
      case s of
        MVarEmpty _ _ -> return Nothing

        -- It's full. If there's at least one thread blocked in putMVar, wake
        -- up the first one leaving the MVar full with the new put value.
        -- Otherwise the MVar becomes empty.
        MVarFull x putq ->
          case Deque.uncons putq of
            Nothing -> do
              writeTVar tv (MVarEmpty mempty mempty)
              return (Just x)

            Just ((x', putvar), putq') -> do
              writeTVar putvar True
              writeTVar tv (MVarFull x' putq')
              return (Just x)


-- | 'readMVarDefault' when the 'MVar' is empty, guarantees to receive next
-- 'putMVar' value.  It will also not block if the 'MVar' is full, even if there
-- are other threads attempting to 'putMVar'.
--
readMVarDefault :: ( MonadLabelledSTM m
                   , MonadMask m
                   , forall x tvar. tvar ~ TVar m x => Eq tvar
                   )
                => MVarDefault m a
                -> m a
readMVarDefault (MVar tv) = do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- It's empty, add ourselves to the 'read' blocked queue.
        MVarEmpty takeq readq -> do
          readvar <- newTVar Nothing
          labelTVar readvar "internal-readvar"
          writeTVar tv (MVarEmpty takeq (Deque.snoc readvar readq))
          return (Left readvar)

        -- if it's full return the value
        MVarFull x _ -> return (Right x)

    case res of
      -- we have to block on our own readvar until we can complete the read
      Left readvar ->
        atomically (readTVar readvar >>= maybe retry return)
        `catch` \e@SomeAsyncException {} -> do
          atomically $ do
            s <- readTVar tv
            case s of
              MVarEmpty takeq readq -> do
                -- async exception was thrown while were were blocked on
                -- readvar; we need to remove it from 'readq', otherwise we
                -- will have a space leak.
                let readq' = Deque.filter (/= readvar) readq
                writeTVar tv (MVarEmpty takeq readq')

              -- This case is unlikely but possible if another thread ran
              -- first and modified the mvar. This situation is fine as far as
              -- space leaks are concerned because it means our wait var is no
              -- longer in the wait queue.
              MVarFull {} -> return ()
          throwIO e

      -- we managed to do the take synchronously
      Right x -> return x


tryReadMVarDefault :: MonadSTM m
                   => MVarDefault m a -> m (Maybe a)
tryReadMVarDefault (MVar tv) =
    atomically $ do
      s <- readTVar tv
      case s of
        MVarFull  x _ -> return (Just x)
        MVarEmpty {}  -> return Nothing


isEmptyMVarDefault :: MonadSTM  m
                   => MVarDefault m a -> m Bool
isEmptyMVarDefault (MVar tv) =
    atomically $ do
      s <- readTVar tv
      case s of
        MVarFull  {} -> return False
        MVarEmpty {} -> return True
