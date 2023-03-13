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

  type MVar m :: Type -> Type

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
                                 !(Deque (TVar m (Maybe a))) -- ^ blocked on read
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
newEmptyMVarDefault = MVar <$> newTVarIO (MVarEmpty mempty mempty)


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
        -- if it's full we add ourselves to the 'put' blocked queue
        MVarFull x' putq -> do
          putvar <- newTVar False
          writeTVar tv (MVarFull x' (Deque.snoc (x, putvar) putq))
          return (Just putvar)

        -- if it's empty we fill in the value, and also complete the action of
        -- the next thread blocked in takeMVar
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
                   , MonadSTM  m
                   , forall x tvar. tvar ~ TVar m x => Eq tvar
                   )
                => MVarDefault m a
                -> m a
takeMVarDefault (MVar tv) = mask_ $ do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- if it's empty we add ourselves to the 'take' blocked queue
        MVarEmpty takeq readq -> do
          takevar <- newTVar Nothing
          writeTVar tv (MVarEmpty (Deque.snoc takevar takeq) readq)
          return (Left takevar)

        -- if it's full we grab the value, and also complete the action of the
        -- next thread blocked in putMVar, by setting the new MVar value and
        -- unblocking them.
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
                writeTVar tv (MVarEmpty takeq' readq)

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
readMVarDefault :: ( MonadSTM m
                   , MonadMask m
                   , forall x tvar. tvar ~ TVar m x => Eq tvar
                   ) 
                => MVarDefault m a
                -> m a
readMVarDefault (MVar tv) = do
    res <- atomically $ do
      s <- readTVar tv
      case s of
        -- if it's empty we add ourselves to the 'read' blocked queue
        MVarEmpty takeq readq -> do
          readvar <- newTVar Nothing
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
