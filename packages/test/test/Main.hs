module Main where

import Chiasma.Test.CodecTest (test_decode, test_query)
import Chiasma.Test.LensTest (test_lenses)
import Chiasma.Test.OutputParseTest (test_outputParse)
import Chiasma.Test.PinTest (test_pin)
import Chiasma.Test.PureTest (test_pureClient)
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
    unitTest "pure client" test_pureClient
  ]

main :: IO ()
main =
  defaultMain tests
