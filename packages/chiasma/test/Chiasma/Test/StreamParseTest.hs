module Chiasma.Test.StreamParseTest where

import Chiasma.Native.StreamParse (TmuxOutputBlock(Success), parseConduit)
import Conduit
import Data.ByteString.Internal (packChars)
import Data.Conduit.List
import qualified Data.Text as T (unpack)
import Hedgehog ((===))

import Chiasma.Test.Util (UnitTest)

paneLine :: Text
paneLine = "%0 100 100"

tmuxOutput :: [Text]
tmuxOutput = [
  "%begin 123\n",
  "%end 123\n",
  "%session-changed $0 0\n%begin 234\n",
  paneLine <> "\nb\n%end 234\n",
  "%begin 345\n",
  "c\n",
  "d\n",
  "%end 345\n"
  ]

test_streamParse :: UnitTest
test_streamParse = do
  r <- runConduit $ sourceList tmuxOutput .| mapC (packChars . T.unpack) .| parseConduit .| sinkList
  [Success [], Success [paneLine, "b"], Success ["c", "d"]] === r
