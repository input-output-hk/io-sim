{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | A generalisation of the
-- <https://hackage.haskell.org/package/base/docs/Data-Unique.html Data.Unique>
-- API to both 'IO' and <https://hackage.haskell.org/package/io-sim IOSim>.
--
module Control.Monad.Class.MonadUnique
  ( MonadUnique (..)
  , UniqueFor (..)
  ) where

-- base
import Data.Kind (Type)
import Data.Unique qualified as IO

-- transformers
import Control.Monad.Reader (MonadTrans (..), ReaderT (..), lift)


class (Monad m, Eq (Unique m), Ord (Unique m)) => MonadUnique m where
  type Unique m = (unique :: Type) | unique -> m
  newUnique  :: m (Unique m)
  hashUnique :: Unique m -> Int

  default
    newUnique
      :: (m ~ t n, Unique m ~ UniqueFor t n, MonadTrans t, MonadUnique n)
      => m (Unique m)
  default
    hashUnique
      :: (m ~ t n, Unique m ~ UniqueFor t n, MonadUnique n)
      => Unique m -> Int
  newUnique  = lift (MkUniqueFor <$> newUnique)
  hashUnique = hashUnique . unMkUniqueFor

instance MonadUnique IO where
  type Unique IO = IO.Unique
  newUnique  = IO.newUnique
  hashUnique = IO.hashUnique


newtype UniqueFor t m = MkUniqueFor{ unMkUniqueFor :: Unique m }
deriving instance MonadUnique m => Eq  (UniqueFor r m)
deriving instance MonadUnique m => Ord (UniqueFor r m)

instance MonadUnique m => MonadUnique (ReaderT r m) where
  type Unique (ReaderT r m) = UniqueFor (ReaderT r) m
