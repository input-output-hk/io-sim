module Main (main) where

import           Test.Tasty

import qualified Test.Control.Monad.IOSim.Test (tests)
import qualified Test.Control.Monad.IOSimPOR.Test (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "IO Sim"
  [ Test.Control.Monad.IOSim.Test.tests
  , Test.Control.Monad.IOSimPOR.Test.tests
  ]
