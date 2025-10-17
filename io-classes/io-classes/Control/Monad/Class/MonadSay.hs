module Control.Monad.Class.MonadSay where

import Control.Monad.Reader
import Data.ByteString.Char8 qualified as BSC

class Monad m => MonadSay m where
  say :: String -> m ()

instance MonadSay IO where
  say = BSC.putStrLn . BSC.pack

instance MonadSay m => MonadSay (ReaderT r m) where
  say = lift . say
