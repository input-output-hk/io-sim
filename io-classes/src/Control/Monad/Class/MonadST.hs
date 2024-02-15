{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Class.MonadST (MonadST (..)) where

import Control.Monad.Reader

import Control.Monad.Primitive
import Control.Monad.ST as ST (ST, stToIO)


-- | This class is for abstracting over 'stToIO' which allows running 'ST'
-- actions in 'IO'. In this case it is to allow running 'ST' actions within
-- another monad @m@.
--
-- The normal type of 'stToIO' is:
--
-- > stToIO :: ST RealWorld a -> IO a
--
-- We have two approaches to abstracting over this, a new and an older
-- (deprecated) method. The new method borrows the @primitive@ package's
-- 'PrimMonad' and 'PrimState' type family. This gives us:
--
-- > stToIO :: ST (PrimState m) a -> m a
--
-- Which for 'IO' is exactly the same as above. For 'ST' it is identity, while
-- for @IOSim@ it is
--
-- > stToIO :: ST s a -> IOSim s a
--
-- The older (deprecated) method is tricky because we need to not care about
-- both the @IO@, and also the @RealWorld@, and it does so avoiding mentioning
-- any @s@ type (which is what the 'PrimState' type family gives access to).
-- The solution is to write an action that is given the @liftST@ as an argument
-- and where that action itself is polymorphic in the @s@ parameter. This
-- allows us to instantiate it with @RealWorld@ in the @IO@ case, and the local
-- @s@ in a case where we are embedding into another @ST@ action.
--
class Monad m => MonadST m where
  -- | @since 1.4.1.0
  stToIO :: ST (PrimState m) a -> m a

  -- | Deprecated. Use 'stToIO' instead.
  withLiftST :: (forall s. (forall a. ST s a -> m a) -> b) -> b

{-# DEPRECATED withLiftST "Use the simpler 'stToIO' instead." #-}

instance MonadST IO where
  stToIO = stToPrim
  withLiftST = \f -> f ST.stToIO

instance MonadST (ST s) where
  stToIO = stToPrim
  withLiftST = \f -> f id

instance (MonadST m, PrimMonad m) => MonadST (ReaderT r m) where
  stToIO :: ST (PrimState m) a -> ReaderT r m a
  stToIO f = lift (stToPrim f)

  withLiftST f = withLiftST $ \g -> f (lift . g)
