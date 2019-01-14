{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StreamParseSpec(
  htf_thisModulesTests,
) where

import Conduit
import Data.ByteString.Internal (packChars)
import Data.Conduit.List
import Test.Framework
import Chiasma.Native.StreamParse (parseConduit)

paneLine :: String
paneLine = "%0 100 100"

tmuxOutput :: [String]
tmuxOutput = [
  "%begin 123\n",
  "%end 123\n",
  "%session-changed $0 0\n%begin 234\n",
  paneLine ++ "\nb\n%end 234\n",
  "%begin 345\n",
  "c\n",
  "d\n",
  "%end 345\n"
  ]

test_conduit :: IO ()
test_conduit = do
  r <- runConduit $ sourceList tmuxOutput .| mapC packChars .| parseConduit .| sinkList
  assertEqual [[], [paneLine, "b"], ["c", "d"]] r
