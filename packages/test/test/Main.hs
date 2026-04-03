module Main where

import Chiasma.Test.CodecTest (test_decode, test_query)
import Chiasma.Test.LensTest (test_lenses)
import Chiasma.Test.OutputParseTest (test_outputParse)
import Chiasma.Test.PinTest (test_pin)
import Chiasma.Test.ProcessOutputTest (test_bufferContinuation, test_multiNotificationChunk,
  test_responseAndNotificationChunk)
import Chiasma.Test.Util (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "tmux-decode a value" test_decode,
    unitTest "tmux-encode a query" test_query,
    test_lenses,
    unitTest "parse tmux output" test_outputParse,
    test_pin,
    unitTest "multiple notifications in one chunk" test_multiNotificationChunk,
    unitTest "response and notification in one chunk" test_responseAndNotificationChunk,
    unitTest "buffer continuation across chunks" test_bufferContinuation
  ]

main :: IO ()
main =
  defaultMain tests
