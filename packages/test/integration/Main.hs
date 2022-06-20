module Main where

import Chiasma.Test.CaptureTest (test_capture)
import Chiasma.Test.RenderTest (test_render)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Chiasma.Test.Util (integrationTest)

tests :: TestTree
tests =
  testGroup "all" [
    test_render,
    integrationTest "capture pane content" test_capture
  ]

main :: IO ()
main =
  defaultMain tests
