{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.IOSimPOR.QuickCheckUtils where

import Control.Monad.ST.Lazy
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

-- We also need a version of conjoin that is sequential, but does not
-- label its result as an IO property unless one of its arguments
-- is. Consequently it does not catch exceptions in its arguments.

conjoinNoCatchST :: TestableNoCatch prop => [ST s prop] -> ST s Property
conjoinNoCatchST sts = do
    ps <- sequence sts
    return $ conjoinNoCatch ps

conjoinNoCatch :: TestableNoCatch prop => [prop] -> Property
conjoinNoCatch = conjoinSpeculate id

conjoinSpeculate :: TestableNoCatch prop => ([Rose Result] -> [Rose Result]) -> [prop] -> Property
conjoinSpeculate spec ps =
  againNoCatch $
  MkProperty $
  do roses <- mapM (fmap unProp . unProperty . propertyNoCatch) ps
     return (MkProp $ conj id (spec roses))
 where

  conj k [] =
    MkRose (k succeeded) []

  conj k (p : ps) = do
    result <- p
    case ok result of
      _ | not (expect result) ->
        return failed { reason = "expectFailure may not occur inside a conjunction" }
      Just True -> conj (addLabels result . addCallbacksAndCoverage result . k) ps
      Just False -> p
      Nothing -> do
        let rest = conj (addCallbacksAndCoverage result . k) ps
        result2 <- rest
        -- Nasty work to make sure we use the right callbacks
        case ok result2 of
          Just True  -> MkRose (result2 { ok = Nothing }) []
          Just False -> rest
          Nothing    -> rest

  addCallbacksAndCoverage result r =
    r { callbacks = callbacks result ++ callbacks r,
        requiredCoverage = requiredCoverage result ++ requiredCoverage r }
  addLabels result r =
    r { labels = labels result ++ labels r,
        classes = classes result ++ classes r,
        tables = tables result ++ tables r }


-- .&&| is a sequential, but parallelism-friendly version of .&&., that
-- tests its arguments in sequence, but does not label its result as
-- an IO property unless one of its arguments is.

infixr 1 .&&|
(.&&|) :: TestableNoCatch prop => prop -> prop -> Property
p .&&| q = conjoinNoCatch [p, q]


-- property catches exceptions in its argument, turning everything
-- Testable into an IORose property, which cannot be paralellized. We
-- need an alternative that permits parallelism by allowing exceptions
-- to propagate. This is a modified clone of code from
-- Test.QuickCheck.Property.

class TestableNoCatch prop where
  propertyNoCatch :: prop -> Property

instance TestableNoCatch Discard where
  propertyNoCatch _ = propertyNoCatch rejected

instance TestableNoCatch Bool where
  propertyNoCatch = propertyNoCatch . liftBool

instance TestableNoCatch Result where
  propertyNoCatch = MkProperty . return . MkProp . return

instance TestableNoCatch Prop where
  propertyNoCatch = MkProperty . return

instance TestableNoCatch prop => TestableNoCatch (Gen prop) where
  propertyNoCatch mp = MkProperty $ do p <- mp; unProperty (againNoCatch $ propertyNoCatch p)

instance TestableNoCatch Property where
  propertyNoCatch p = p

againNoCatch :: Property -> Property
againNoCatch (MkProperty gen) = MkProperty $ do
  MkProp rose <- gen
  return . MkProp $ fmap (\res -> res{ abort = False }) rose
