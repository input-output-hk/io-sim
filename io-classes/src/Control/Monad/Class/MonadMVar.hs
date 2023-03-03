{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Monad.Class.MonadMVar
  ( MonadMVar (..)
  , MVarDefault
  , newEmptyMVarDefault
  , newMVarDefault
  , putMVarDefault
  , takeMVarDefault
  , readMVarDefault
  , tryTakeMVarDefault
  , tryPutMVarDefault
  , isEmptyMVarDefault
  ) where

import           Control.Concurrent.Class.MonadSTM.TVar
import qualified Control.Concurrent.MVar as IO
import           Control.Exception (SomeAsyncException (..))
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans (lift)

import           Data.Kind (Type)
import           Deque.Strict (Deque)
import qualified Deque.Strict as Deque


class Monad m => MonadMVar m where
  {-# MINIMAL newEmptyMVar, takeMVar, putMVar, tryTakeMVar, tryPutMVar, isEmptyMVar #-}

  type MVar m = (mvar :: Type -> Type) | mvar -> m

  newEmptyMVar      :: m (MVar m a)
  takeMVar          :: MVar m a -> m a
  putMVar           :: MVar m a -> a -> m ()
  tryTakeMVar       :: MVar m a -> m (Maybe a)
  tryPutMVar        :: MVar m a -> a -> m Bool
  isEmptyMVar       :: MVar m a -> m Bool

  -- methods with a default implementation
  newMVar           :: a -> m (MVar m a)
  readMVar          :: MVar m a -> m a
  swapMVar          :: MVar m a -> a -> m a
  withMVar          :: MVar m a -> (a -> m b) -> m b
  withMVarMasked    :: MVar m a -> (a -> m b) -> m b
  modifyMVar_       :: MVar m a -> (a -> m a) -> m ()
  modifyMVar        :: MVar m a -> (a -> m (a, b)) -> m b
  modifyMVarMasked_ :: MVar m a -> (a -> m a) -> m ()
  modifyMVarMasked  :: MVar m a -> (a -> m (a,b)) -> m b

  default newMVar :: a -> m (MVar m a)
  newMVar a = do
    v <- newEmptyMVar
    putMVar v a
    return v
  {-# INLINE newMVar #-}

  default readMVar :: MVar m a -> m a
  readMVar v = do
    a <- takeMVar v
    putMVar v a
    return a
  {-# INLINE readMVar #-}

  default swapMVar :: MonadMask m => MVar m a -> a -> m a
  swapMVar mvar new =
    mask_ $ do
      old <- takeMVar mvar
      putMVar mvar new
      return old
  {-# INLINE swapMVar #-}

  default withMVar :: MonadMask m => MVar m a -> (a -> m b) -> m b
  withMVar m io =
    mask $ \restore -> do
      a <- takeMVar m
      b <- restore (io a) `onException` putMVar m a
      putMVar m a
      return b
  {-# INLINE withMVar #-}

  default withMVarMasked :: MonadMask m => MVar m a -> (a -> m b) -> m b
  withMVarMasked m io =
    mask_ $ do
      a <- takeMVar m
      b <- io a `onException` putMVar m a
      putMVar m a
      return b
  {-# INLINE withMVarMasked #-}

  default modifyMVar_ :: MonadMask m => MVar m a -> (a -> m a) -> m ()
  modifyMVar_ m io =
    mask $ \restore -> do
      a  <- takeMVar m
      a' <- restore (io a) `onException` putMVar m a
      putMVar m a'
  {-# INLINE modifyMVar_ #-}

  default modifyMVar :: (MonadMask m, MonadEvaluate m)
                     => MVar m a -> (a -> m (a,b)) -> m b
  modifyMVar m io =
    mask $ \restore -> do
      a      <- takeMVar m
      (a',b) <- restore (io a >>= evaluate) `onException` putMVar m a
      putMVar m a'
      return b
  {-# INLINE modifyMVar #-}

  default modifyMVarMasked_ :: MonadMask m => MVar m a -> (a -> m a) -> m ()
  modifyMVarMasked_ m io =
    mask_ $ do
      a  <- takeMVar m
      a' <- io a `onException` putMVar m a
      putMVar m a'
  {-# INLINE modifyMVarMasked_ #-}

  default modifyMVarMasked :: (MonadMask m, MonadEvaluate m)
                           => MVar m a -> (a -> m (a,b)) -> m b
  modifyMVarMasked m io =
    mask_ $ do
      a      <- takeMVar m
      (a',b) <- (io a >>= evaluate) `onException` putMVar m a
      putMVar m a'
      return b
  {-# INLINE modifyMVarMasked #-}


instance MonadMVar IO where
    type MVar IO      = IO.MVar
    newEmptyMVar      = IO.newEmptyMVar
    newMVar           = IO.newMVar
    takeMVar          = IO.takeMVar
    putMVar           = IO.putMVar
    readMVar          = IO.readMVar
    swapMVar          = IO.swapMVar
    tryTakeMVar       = IO.tryTakeMVar
    tryPutMVar        = IO.tryPutMVar
    isEmptyMVar       = IO.isEmptyMVar
    withMVar          = IO.withMVar
    withMVarMasked    = IO.withMVarMasked
    modifyMVar_       = IO.modifyMVar_
    modifyMVar        = IO.modifyMVar
    modifyMVarMasked_ = IO.modifyMVarMasked_
    modifyMVarMasked  = IO.modifyMVarMasked


data MVarState m a = MVarEmpty   !(Deque (TVar m (Maybe a))) -- ^ blocked on take
                   | MVarFull  a !(Deque (a, TVar m Bool))   -- ^ blocked on put

-- | A default 'MVar' implementation based on `TVar`'s.  An 'MVar' provides
-- fairness guarantees.
--
-- /Implementation details:/
--
-- 'STM' does not guarantee fairness, instead it provide compositionally.
-- Fairness of 'putMVarDefault' and 'takeMVarDefault' is provided by tracking
-- queue of blocked operation in the 'MVarState', e.g.  when a 'putMVarDefault'
-- is scheduled on a full 'MVar', the request is put on to the back of the queue
-- together with a wakeup var.  When 'takeMVarDefault' executes, it returns the
-- value and is using the first element of the queue to set the new value of
-- the 'MVar' and signals next `putMVarDefault` operation to unblock.  This has
-- an effect as if all the racing `putMVarDefault` calls where executed in
-- turns.
--
newtype MVarDefault m a = MVar (TVar m (MVarState m a))


newEmptyMVarDefault :: MonadSTM m => m (MVarDefault m a)
newEmptyMVarDefault = MVar <$> newTVarIO (MVarEmpty mempty)


newMVarDefault :: MonadSTM m => a -> m (MVarDefault m a)
newMVarDefault a = MVar <$> newTVarIO (MVarFull a mempty)


putMVarDefault :: ( MonadCatch m
                  , MonadMask  m
                  , MonadSTM   m
                  , forall x tvar. tvar ~ TVar m x => Eq tvar
                  )
               => MVarDefault m a -> a -> m ()
putMVarDefault (MVar tv) x = mask_ $ do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- if it's full we add ourselves to the blocked queue
        MVarFull x' blockedq -> do
          wakevar <- newTVar False
          writeTVar tv (MVarFull x' (Deque.snoc (x, wakevar) blockedq))
          return (Just wakevar)

        -- if it's empty we fill in the value, and also complete the action of
        -- the next thread blocked in takeMVar
        MVarEmpty blockedq ->
          case Deque.uncons blockedq of
            Nothing -> do
              writeTVar tv (MVarFull x mempty)
              return Nothing

            Just (wakevar, blockedq') -> do
              writeTVar wakevar (Just x)
              writeTVar tv (MVarEmpty blockedq')
              return Nothing

    case res of
      -- we have to block on our own wakevar until we can complete the put
      Just wakevar ->
        atomically (readTVar wakevar >>= check)
        `catch` \e@SomeAsyncException {} -> do
          atomically $ do
            s <- readTVar tv
            case s of
              MVarFull x' blockedq -> do
                -- async exception was thrown while we were blocked on wakevar;
                -- we need to remove it from the queue, otherwise we will have
                -- a space leak.
                let blockedq' = Deque.filter ((/= wakevar) . snd) blockedq
                writeTVar tv (MVarFull x' blockedq')
              -- the exception was thrown when we were blocked on 'waketvar', so
              -- the 'MVar' must not be empty.
              MVarEmpty {} -> error "putMVarDefault: invariant violation"
          throwIO e

      -- we managed to do the put synchronously
      Nothing -> return ()


takeMVarDefault :: ( MonadMask m
                   , MonadSTM  m
                   , forall x tvar. tvar ~ TVar m x => Eq tvar
                   )
                => MVarDefault m a
                -> m a
takeMVarDefault (MVar tv) = mask_ $ do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- if it's empty we add ourselves to the blocked queue
        MVarEmpty blockedq -> do
          wakevar <- newTVar Nothing
          writeTVar tv (MVarEmpty (Deque.snoc wakevar blockedq))
          return (Left wakevar)

        -- if it's full we grab the value, and also complete the action of the
        -- next thread blocked in putMVar, by setting the new MVar value and
        -- unblocking them.
        MVarFull x blockedq ->
          case Deque.uncons blockedq of
            Nothing -> do
              writeTVar tv (MVarEmpty mempty)
              return (Right x)

            Just ((x', wakevar), blockedq') -> do
              writeTVar wakevar True
              writeTVar tv (MVarFull x' blockedq')
              return (Right x)

    case res of
      -- we have to block on our own wakevar until we can complete the read
      Left wakevar ->
        atomically (readTVar wakevar >>= maybe retry return)
        `catch` \e@SomeAsyncException {} -> do
          atomically $ do
            s <- readTVar tv
            case s of
              MVarEmpty blockedq -> do
                -- async exception was thrown while were were blocked on
                -- wakevar; we need to remove it from 'blockedq', otherwise we
                -- will have a space leak.
                let blockedq' = Deque.filter (/= wakevar) blockedq
                writeTVar tv (MVarEmpty blockedq')
              -- the exception was thrown while we were blocked on 'wakevar', so
              -- the 'MVar' must not be full.
              MVarFull {} -> error "takeMVarDefault: invariant violation"
          throwIO e

      -- we managed to do the take synchronously
      Right x -> return x


tryPutMVarDefault :: MonadSTM m
                  => MVarDefault m a -> a -> m Bool
tryPutMVarDefault (MVar tv) x =
    atomically $ do
      s <- readTVar tv
      case s of
        MVarFull {} -> return False

        MVarEmpty blockedq ->
          case Deque.uncons blockedq of
            Nothing -> do
              writeTVar tv (MVarFull x mempty)
              return True

            Just (wakevar, blockedq') -> do
              writeTVar wakevar (Just x)
              writeTVar tv (MVarEmpty blockedq')
              return True


-- | 'readMVarDefault' when the 'MVar' is empty, guarantees to receive next
-- 'putMVar' value.  It will also not block if the 'MVar' is full, even if there
-- are other threads attempting to 'putMVar'.
--
readMVarDefault :: MonadSTM m
                => MVarDefault m a
                -> m a
readMVarDefault (MVar tv) = do
    atomically $ do
      s <- readTVar tv
      case s of
        -- if it's empty block
        MVarEmpty {} -> retry

        -- if it's full return the value
        MVarFull x _ -> return x


tryTakeMVarDefault :: MonadSTM m
                   => MVarDefault m a
                   -> m (Maybe a)
tryTakeMVarDefault (MVar tv) = do
    atomically $ do
      s <- readTVar tv
      case s of
        MVarEmpty _ -> return Nothing
        MVarFull x blockedq ->
          case Deque.uncons blockedq of
            Nothing -> do
              writeTVar tv (MVarEmpty mempty)
              return (Just x)

            Just ((x', wakevar), blockedq') -> do
              writeTVar wakevar True
              writeTVar tv (MVarFull x' blockedq')
              return (Just x)


isEmptyMVarDefault :: MonadSTM  m
                   => MVarDefault m a -> m Bool
isEmptyMVarDefault (MVar tv) =
    atomically $ do
      s <- readTVar tv
      case s of
        MVarFull  {} -> return False
        MVarEmpty {} -> return True


--
-- ReaderT instance
--

newtype WrappedMVar r (m :: Type -> Type) a = WrappedMVar { unwrapMVar :: MVar m a }

instance ( MonadMask m
         , MonadMVar m
         , MonadEvaluate m
         ) => MonadMVar (ReaderT r m) where
    type MVar (ReaderT r m) = WrappedMVar r m
    newEmptyMVar = WrappedMVar <$> lift newEmptyMVar
    newMVar      = fmap WrappedMVar . lift . newMVar
    takeMVar     = lift .   takeMVar    . unwrapMVar
    putMVar      = lift .: (putMVar     . unwrapMVar)
    readMVar     = lift .   readMVar    . unwrapMVar
    swapMVar     = lift .: (swapMVar    . unwrapMVar)
    tryTakeMVar  = lift .   tryTakeMVar . unwrapMVar
    tryPutMVar   = lift .: (tryPutMVar  . unwrapMVar)
    isEmptyMVar  = lift .   isEmptyMVar . unwrapMVar
    withMVar (WrappedMVar v) f = ReaderT $ \r ->
      withMVar v (\a -> runReaderT (f a) r)
    withMVarMasked (WrappedMVar v) f = ReaderT $ \r ->
      withMVarMasked v (\a -> runReaderT (f a) r)
    modifyMVar_ (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVar_ v (\a -> runReaderT (f a) r)
    modifyMVar (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVar v (\a -> runReaderT (f a) r)
    modifyMVarMasked_ (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVarMasked_ v (\a -> runReaderT (f a) r)
    modifyMVarMasked (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVarMasked v (\a -> runReaderT (f a) r)




--
-- Utilities
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
