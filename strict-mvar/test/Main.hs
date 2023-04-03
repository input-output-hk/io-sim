module Main where

import qualified Test.Control.Concurrent.Class.MonadMVar.Strict.Checked as Checked
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "Test" [
        testGroup "Control" [
            testGroup "Concurrent" [
                testGroup "Class" [
                    testGroup "MonadMVar" [
                        testGroup "Strict" [
                            testGroup "Checked" [
                                Checked.tests
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]
