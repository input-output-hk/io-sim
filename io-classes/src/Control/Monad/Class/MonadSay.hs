module Control.Monad.Class.MonadSay where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC

class Monad m => MonadSay m where
  say :: String -> m ()

instance MonadSay IO where
  say = BSC.putStrLn . BSC.pack

instance MonadSay m => MonadSay (ReaderT r m) where
  say = lift . say
