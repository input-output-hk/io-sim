module Control.Monad.IOSimPOR.Types where

import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.IOSim.CommonTypes

--
-- Effects
--

-- | An `Effect` aggregates effects performed by a thread.  Only used by
-- *IOSimPOR*.
--
data Effect = Effect {
    effectReads        :: !(Set TVarId),
    effectWrites       :: !(Set TVarId),
    effectForks        :: !(Set ThreadId),
    effectThrows       :: ![ThreadId],
    effectWakeup       :: ![ThreadId],
    effectStatusReads  :: ![ThreadId],
    effectStatusWrites :: ![ThreadId]
  }
  deriving (Eq, Show)

instance Semigroup Effect where
  Effect r w s ts wu sr sw <> Effect r' w' s' ts' wu' sr' sw' =
    Effect (r <> r') (w <> w') (s <> s') (ts ++ ts') (wu++wu') (sr ++ sr') (sw ++ sw')

instance Monoid Effect where
  mempty = Effect Set.empty Set.empty Set.empty [] [] [] []

-- readEffect :: SomeTVar s -> Effect
-- readEffect r = mempty{effectReads = Set.singleton $ someTvarId r }

readEffects :: [SomeTVar s] -> Effect
readEffects rs = mempty{effectReads = Set.fromList (map someTvarId rs)}

-- writeEffect :: SomeTVar s -> Effect
-- writeEffect r = mempty{effectWrites = Set.singleton $ someTvarId r }

writeEffects :: [SomeTVar s] -> Effect
writeEffects rs = mempty{effectWrites = Set.fromList (map someTvarId rs)}

forkEffect :: ThreadId -> Effect
forkEffect tid = mempty{effectForks = Set.singleton $ tid}

throwToEffect :: ThreadId -> Effect
throwToEffect tid = mempty{ effectThrows = [tid] }

wakeupEffects :: [ThreadId] -> Effect
wakeupEffects tids = mempty{effectWakeup = tids}

statusReadEffects :: [ThreadId] -> Effect
statusReadEffects tids = mempty{effectStatusReads = tids}

statusWriteEffect :: ThreadId -> Effect
statusWriteEffect tid = mempty{effectStatusWrites = [tid]}

statusWriteEffects :: [ThreadId] -> Effect
statusWriteEffects tids = mempty{effectStatusWrites = tids}

someTvarId :: SomeTVar s -> TVarId
someTvarId (SomeTVar r) = tvarId r

-- | Make sure we only have read effects
--
-- It seems we need to ignore statusWrites effects to avoid generating bad
-- schedules. See issue #76 for examples.
--
onlyReadEffect :: Effect -> Bool
onlyReadEffect e = e { effectReads = effectReads mempty
                     , effectStatusReads = effectStatusReads mempty
                     -- TODO: This is a quick fix for #76.
                     , effectStatusWrites = effectStatusWrites mempty
                     } == mempty

racingEffects :: Effect -> Effect -> Bool
racingEffects e e' =
      effectThrows e       `intersectsL` effectThrows e'
   || effectReads  e       `intersects`  effectWrites e'
   || effectWrites e       `intersects`  effectReads  e'
   || effectWrites e       `intersects`  effectWrites e'
   || effectStatusReads e  `intersectsL` effectStatusWrites e'
   || effectStatusWrites e `intersectsL` effectStatusReads  e'
   || effectStatusWrites e `intersectsL` effectStatusWrites e'
  where
    intersects :: Ord a => Set a -> Set a -> Bool
    intersects a b = not $ a `Set.disjoint` b

    intersectsL :: Eq a => [a] -> [a] -> Bool
    intersectsL a b = not $ null $ a `List.intersect` b
