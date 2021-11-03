module Chiasma.Test.OutputParseTest where

import Chiasma.Native.Parse (resultLines)
import qualified Data.Text as T (unlines)
import Hedgehog ((===))

import Chiasma.Test.Util (UnitTest)

paneLine :: Text
paneLine = "%0 100 100"

tmuxOutput :: Text
tmuxOutput = T.unlines [
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

test_outputParse :: UnitTest
test_outputParse =
  Right [[], [paneLine, "b"], ["c", "d"]] === resultLines tmuxOutput
