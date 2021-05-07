module Main where

import Chiasma.Test.IndividualProcessTest (test_individual)
import Chiasma.Test.ProcessTest (test_process)
import Chiasma.Test.RenderTest (test_render)
import Chiasma.Test.TmuxStreamTest (test_streamed)
import Chiasma.Test.Util (integrationTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    integrationTest "use individual subprocesses for queries" test_individual,
    integrationTest "tmux control process" test_process,
    test_render,
    integrationTest "streamed tmux commands" test_streamed
  ]

main :: IO ()
main =
  defaultMain tests
