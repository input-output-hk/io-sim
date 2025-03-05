{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | A generalisation of
-- <https://hackage.haskell.org/package/base/docs/Control-Concurrent.html Control.Concurrent>
-- API to both 'IO' and <https://hackage.haskell.org/package/io-sim IOSim>.
--
module Control.Monad.Class.MonadFork
  ( MonadThread (..)
  , labelThisThread
  , MonadFork (..)
  ) where

import Control.Concurrent qualified as IO
import Control.Exception (AsyncException (ThreadKilled), Exception,
           SomeException)
import Control.Monad.Reader (ReaderT (..), lift)
import Data.Kind (Type)
import GHC.Conc.Sync qualified as IO


class (Monad m, Eq   (ThreadId m),
                Ord  (ThreadId m),
                Show (ThreadId m)) => MonadThread m where

  type ThreadId m :: Type

  myThreadId     :: m (ThreadId m)
  labelThread    :: ThreadId m -> String -> m ()

  -- | Requires ghc-9.6.1 or newer.
  --
  -- @since 1.8.0.0
  threadLabel    :: ThreadId m -> m (Maybe String)

-- | Apply the label to the current thread
labelThisThread :: MonadThread m => String -> m ()
labelThisThread label = myThreadId >>= \tid -> labelThread tid label


class MonadThread m => MonadFork m where

  forkIO             :: m () -> m (ThreadId m)
  forkOn             :: Int -> m () -> m (ThreadId m)
  forkIOWithUnmask   :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  forkFinally        :: m a -> (Either SomeException a -> m ()) -> m (ThreadId m)
  throwTo            :: Exception e => ThreadId m -> e -> m ()

  killThread         :: ThreadId m -> m ()
  killThread tid     =  throwTo tid ThreadKilled

  yield              :: m ()

  getNumCapabilities :: m Int


instance MonadThread IO where
  type ThreadId IO = IO.ThreadId
  myThreadId   = IO.myThreadId
  labelThread  = IO.labelThread
#if MIN_VERSION_base(4,18,0)
  threadLabel = IO.threadLabel
#else
  threadLabel = \_ -> pure Nothing
#endif

instance MonadFork IO where
  forkIO             = IO.forkIO
  forkOn             = IO.forkOn
  forkIOWithUnmask   = IO.forkIOWithUnmask
  forkFinally        = IO.forkFinally
  throwTo            = IO.throwTo
  killThread         = IO.killThread
  yield              = IO.yield
  getNumCapabilities = IO.getNumCapabilities

instance MonadThread m => MonadThread (ReaderT r m) where
  type ThreadId (ReaderT r m) = ThreadId m
  myThreadId      = lift myThreadId
  labelThread t l = lift (labelThread t l)
  threadLabel     = lift . threadLabel

instance MonadFork m => MonadFork (ReaderT e m) where
  forkIO (ReaderT f)   = ReaderT $ \e -> forkIO (f e)
  forkOn n (ReaderT f) = ReaderT $ \e -> forkOn n (f e)
  forkIOWithUnmask k   = ReaderT $ \e -> forkIOWithUnmask $ \restore ->
                         let restore' :: ReaderT e m a -> ReaderT e m a
                             restore' (ReaderT f) = ReaderT $ restore . f
                         in runReaderT (k restore') e
  forkFinally f k      = ReaderT $ \e -> forkFinally (runReaderT f e)
                                       $ \err -> runReaderT (k err) e
  throwTo e t = lift (throwTo e t)
  yield       = lift yield

  getNumCapabilities = lift getNumCapabilities
