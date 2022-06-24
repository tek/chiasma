module Main where

import Chiasma.Test.CaptureTest (test_capture)
import Chiasma.Test.FindTest (test_find)
import Chiasma.Test.RenderTest (test_render)
import Chiasma.Test.TmuxTest (test_tmux)
import Chiasma.Test.Util (integrationTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    integrationTest "basic tmux commands" test_tmux,
    integrationTest "find pane by ID" test_find,
    test_render,
    integrationTest "capture pane content" test_capture
  ]

main :: IO ()
main =
  defaultMain tests
