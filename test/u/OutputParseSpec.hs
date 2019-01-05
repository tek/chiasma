{-# OPTIONS_GHC -F -pgmF htfpp #-}

module OutputParseSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Chiasma.Native.Parse (resultLines)

paneLine :: String
paneLine = "%0 100 100"

tmuxOutput :: String
tmuxOutput = unlines [
  "%begin 123",
  "%end 123",
  "%session-changed $0 0",
  "%begin 234",
  paneLine,
  "b",
  "%end 234",
  "%begin 345",
  "c",
  "d",
  "%end 345"
  ]

test_outputParse :: IO ()
test_outputParse =
  assertEqual (Right [[], [paneLine, "b"], ["c", "d"]]) (resultLines tmuxOutput)
