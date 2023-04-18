module Test.Control.Concurrent.Class.MonadMVar.Strict.Checked where

import           Control.Concurrent.Class.MonadMVar.Strict.Checked
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Test.Control.Concurrent.Class.MonadMVar.Strict" [
      testGroup "Checked" [
          testProperty "prop_invariantShouldFail" prop_invariantShouldFail
        , testProperty "prop_invariantShouldNotFail" prop_invariantShouldNotFail
        ]
    ]

-- | Invariant that checks whether an @Int@ is positive.
invPositiveInt :: Int -> Maybe String
invPositiveInt x
  | x >= 0    = Nothing
  | otherwise = Just $ "x<0 for x=" <> show x

prop_invariantShouldFail :: Property
prop_invariantShouldFail = once $ expectFailure $ monadicIO $ run $ do
    v <- newMVarWithInvariant invPositiveInt 0
    modifyMVar_ v (\x -> pure $ x - 1)

prop_invariantShouldNotFail :: Property
prop_invariantShouldNotFail = monadicIO $ run $ do
    v <- newMVarWithInvariant invPositiveInt 0
    modifyMVar_ v (\x -> pure $ x + 1)
