{-# LANGUAGE RankNTypes #-}
module Control.Monad.Class.MonadST (MonadST (..)) where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Control.Monad.ST (ST, stToIO)


-- | This class is for abstracting over 'stToIO' which allows running 'ST'
-- actions in 'IO'. In this case it is to allow running 'ST' actions within
-- another monad @m@.
--
-- The type of 'stToIO' is:
--
-- > stToIO : ST RealWorld a -> IO a
--
-- Abstracting over this is tricky because we need to not care about both
-- the @IO@, and also the @RealWorld@.
--
-- A solution is to write an action that is given the @liftST@ as an argument
-- and where that action itself is polymorphic in the @s@ parameter. This
-- allows us to instantiate it with @RealWorld@ in the @IO@ case, and the local
-- @s@ in a case where we are embedding into another @ST@ action.
--
class Monad m => MonadST m where
  withLiftST :: (forall s. (forall a. ST s a -> m a) -> b) -> b

instance MonadST IO where
  withLiftST = \f -> f stToIO

instance MonadST (ST s) where
  withLiftST = \f -> f id

instance MonadST m => MonadST (ContT r m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (ExceptT e m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (ReaderT r m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (RWST r w s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance MonadST m => MonadST (StateT s m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)

instance (Monoid w, MonadST m) => MonadST (WriterT w m) where
  withLiftST f = withLiftST $ \g -> f (lift . g)
