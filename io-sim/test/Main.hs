module Main (main) where

import           Test.Tasty

import qualified Test.Control.Concurrent.Class.MonadMVar (tests)
import qualified Test.Control.Monad.IOSim (tests)
import qualified Test.Control.Monad.IOSimPOR (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "io-sim-tests"
  [ Test.Control.Concurrent.Class.MonadMVar.tests
  , Test.Control.Monad.IOSim.tests
  , Test.Control.Monad.IOSimPOR.tests
  ]
