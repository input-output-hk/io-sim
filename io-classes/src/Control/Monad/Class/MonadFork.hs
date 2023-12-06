{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Class.MonadFork
  ( MonadThread (..)
  , labelThisThread
  , MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Exception (AsyncException (ThreadKilled), Exception,
                     SomeException)
import           Control.Monad.Reader (ReaderT (..), lift)
import           Data.Kind (Type)
import qualified GHC.Conc.Sync as IO (labelThread)


class (Monad m, Eq   (ThreadId m),
                Ord  (ThreadId m),
                Show (ThreadId m)) => MonadThread m where

  type ThreadId m :: Type

  myThreadId     :: m (ThreadId m)
  labelThread    :: ThreadId m -> String -> m ()

-- | Apply the label to the current thread
labelThisThread :: MonadThread m => String -> m ()
labelThisThread label = myThreadId >>= \tid -> labelThread tid label


class MonadThread m => MonadFork m where

  forkIO           :: m () -> m (ThreadId m)
  forkOn           :: Int -> m () -> m (ThreadId m)
  forkIOWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  forkFinally      :: m a -> (Either SomeException a -> m ()) -> m (ThreadId m)
  throwTo          :: Exception e => ThreadId m -> e -> m ()

  killThread       :: ThreadId m -> m ()
  killThread tid = throwTo tid ThreadKilled

  yield            :: m ()


instance MonadThread IO where
  type ThreadId IO = IO.ThreadId
  myThreadId   = IO.myThreadId
  labelThread  = IO.labelThread

instance MonadFork IO where
  forkIO           = IO.forkIO
  forkOn           = IO.forkOn
  forkIOWithUnmask = IO.forkIOWithUnmask
  forkFinally      = IO.forkFinally
  throwTo          = IO.throwTo
  killThread       = IO.killThread
  yield            = IO.yield

instance MonadThread m => MonadThread (ReaderT r m) where
  type ThreadId (ReaderT r m) = ThreadId m
  myThreadId      = lift myThreadId
  labelThread t l = lift (labelThread t l)

instance MonadFork m => MonadFork (ReaderT e m) where
  forkIO (ReaderT f)   = ReaderT $ \e -> forkIO (f e)
  forkOn n (ReaderT f) = ReaderT $ \e -> forkOn n (f e)
  forkIOWithUnmask k   = ReaderT $ \e -> forkIOWithUnmask $ \restore ->
                         let restore' :: ReaderT e m a -> ReaderT e m a
                             restore' (ReaderT f) = ReaderT $ restore . f
                         in runReaderT (k restore') e
  forkFinally f k     = ReaderT $ \e -> forkFinally (runReaderT f e)
                                      $ \err -> runReaderT (k err) e
  throwTo e t = lift (throwTo e t)
  yield       = lift yield
