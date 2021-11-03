module Main where

import Chiasma.Test.RenderTest (test_render)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    test_render
  ]

main :: IO ()
main =
  defaultMain tests
