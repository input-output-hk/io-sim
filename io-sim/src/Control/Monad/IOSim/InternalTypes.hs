{-# LANGUAGE GADTs #-}

-- | Internal types shared between `IOSim` and `IOSimPOR`.
--
module Control.Monad.IOSim.InternalTypes
  ( ThreadControl (..)
  , ControlStack (..)
  , IsLocked (..)
  , unsafeUnregisterTimeout
  ) where

import Control.Concurrent.Class.MonadSTM
import Control.Exception (Exception)
import Control.Monad.Class.MonadThrow (MaskingState (..))

import Control.Monad.IOSim.Types (IOSim (..), IOSimThreadId, SimA (..),
           TimeoutId)

import GHC.Exts (oneShot)

-- We hide the type @b@ here, so it's useful to bundle these two parts together,
-- rather than having Thread have an existential type, which makes record
-- updates awkward.
data ThreadControl s a where
  ThreadControl :: SimA s b
                -> !(ControlStack s b a)
                -> ThreadControl s a

instance Show (ThreadControl s a) where
  show _ = "..."

data ControlStack s b a where
  MainFrame    :: ControlStack s a  a
  ForkFrame    :: ControlStack s () a
  MaskFrame    :: (b -> SimA s c)         -- subsequent continuation
               -> MaskingState            -- thread local state to restore
               -> !(ControlStack s c a)
               -> ControlStack s b a
  CatchFrame   :: Exception e
               => (e -> SimA s b)         -- exception continuation
               -> (b -> SimA s c)         -- subsequent continuation
               -> !(ControlStack s c a)
               -> ControlStack s b a
  TimeoutFrame :: TimeoutId
               -> TMVar (IOSim s) IOSimThreadId
               -> (Maybe b -> SimA s c)
               -> !(ControlStack s c a)
               -> ControlStack s b a
  DelayFrame   :: TimeoutId
               -> SimA s c
               -> !(ControlStack s c a)
               -> ControlStack s b a
  TouchFrame   :: x
               -> (b -> SimA s c)
               -> !(ControlStack s c a)
               -> ControlStack s b a 

instance Show (ControlStack s b a) where
  show = show . dash
    where
      dash :: ControlStack s b' a -> ControlStackDash
      dash MainFrame                  = MainFrame'
      dash ForkFrame                  = ForkFrame'
      dash (MaskFrame _ m cs)         = MaskFrame' m (dash cs)
      dash (CatchFrame _ _ cs)        = CatchFrame' (dash cs)
      dash (TimeoutFrame tmid _ _ cs) = TimeoutFrame' tmid (dash cs)
      dash (DelayFrame tmid _ cs)     = DelayFrame' tmid (dash cs)
      dash (TouchFrame _ _ cs)        = TouchFrame' (dash cs)

data ControlStackDash =
    MainFrame'
  | ForkFrame'
  | MaskFrame' MaskingState ControlStackDash
  | CatchFrame' ControlStackDash
  -- TODO: Figure out a better way to include IsLocked here
  | TimeoutFrame' TimeoutId ControlStackDash
  | ThreadDelayFrame' TimeoutId ControlStackDash
  | DelayFrame' TimeoutId ControlStackDash
  | TouchFrame' ControlStackDash
  deriving Show

data IsLocked = NotLocked | Locked !IOSimThreadId
  deriving (Eq, Show)

-- | Unsafe method which removes a timeout.
--
-- It's not part of public API, and it might cause deadlocks when used in
-- a wrong context.
--
-- It is defined here rather so that it's not exposed to the user, even tough
-- one could define it oneself.
--
-- TODO: `SimA` constructors should be defined here.
--
unsafeUnregisterTimeout :: TimeoutId -> IOSim s ()
unsafeUnregisterTimeout tmid = IOSim $ oneShot $ \k -> UnregisterTimeout tmid (k ())
