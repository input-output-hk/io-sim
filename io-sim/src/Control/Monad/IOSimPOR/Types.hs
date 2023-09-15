{-# LANGUAGE NamedFieldPuns #-}
module Control.Monad.IOSimPOR.Types where

import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.IOSim.CommonTypes

--
-- Effects
--

-- | An `Effect` aggregates effects performed by a thread in a given
-- execution step.  Only used by *IOSimPOR*.
--
data Effect = Effect {
    effectReads  :: !(Set TVarId),
    effectWrites :: !(Set TVarId),
    effectForks  :: !(Set ThreadId),
    effectThrows :: ![ThreadId],
    effectWakeup :: ![ThreadId]
  }
  deriving Eq

instance Show Effect where
    show Effect { effectReads, effectWrites, effectForks, effectThrows, effectWakeup } =
      concat $ [ "Effect { " ]
            ++ [ "reads = " ++ show effectReads ++ ", "   | not (null effectReads) ]
            ++ [ "writes = " ++ show effectWrites ++ ", " | not (null effectWrites) ]
            ++ [ "forks = " ++ show effectForks ++ ", "   | not (null effectForks)]
            ++ [ "throws = " ++ show effectThrows ++ ", " | not (null effectThrows) ]
            ++ [ "wakeup = " ++ show effectWakeup ++ ", " | not (null effectWakeup) ]
            ++ [ "}" ]


instance Semigroup Effect where
  Effect r w s ts wu <> Effect r' w' s' ts' wu' =
    Effect (r <> r') (w <> w') (s <> s') (ts ++ ts') (wu++wu')

instance Monoid Effect where
  mempty = Effect Set.empty Set.empty Set.empty [] []

--
-- Effect smart constructors
--

-- readEffect :: SomeTVar s -> Effect
-- readEffect r = mempty{effectReads = Set.singleton $ someTvarId r }

readEffects :: [SomeTVar s] -> Effect
readEffects rs = mempty{effectReads = Set.fromList (map someTvarId rs)}

-- writeEffect :: SomeTVar s -> Effect
-- writeEffect r = mempty{effectWrites = Set.singleton $ someTvarId r }

writeEffects :: [SomeTVar s] -> Effect
writeEffects rs = mempty{effectWrites = Set.fromList (map someTvarId rs)}

forkEffect :: ThreadId -> Effect
forkEffect tid = mempty{effectForks = Set.singleton tid}

throwToEffect :: ThreadId -> Effect
throwToEffect tid = mempty{ effectThrows = [tid] }

wakeupEffects :: [ThreadId] -> Effect
wakeupEffects tids = mempty{effectWakeup = tids}

--
-- Utils
--

someTvarId :: SomeTVar s -> TVarId
someTvarId (SomeTVar r) = tvarId r

onlyReadEffect :: Effect -> Bool
onlyReadEffect e = e { effectReads = effectReads mempty } == mempty

-- | Check if two effects race.  The two effects are assumed to come from
-- different threads, from steps which do not wake one another, see
-- `racingSteps`.
--
racingEffects :: Effect -> Effect -> Bool
racingEffects e e' =

       -- both effects throw to the same threads
       effectThrows e `intersectsL` effectThrows e'

       -- concurrent reads & writes of the same TVars
    || effectReads  e `intersects`  effectWrites e'
    || effectWrites e `intersects`  effectReads  e'

       -- concurrent writes to the same TVars
    || effectWrites e `intersects`  effectWrites e'

  where
    intersects :: Ord a => Set a -> Set a -> Bool
    intersects a b = not $ a `Set.disjoint` b

    intersectsL :: Eq a => [a] -> [a] -> Bool
    intersectsL a b = not $ null $ a `List.intersect` b
