{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StreamParseSpec(
  htf_thisModulesTests,
) where

import Conduit
import Data.ByteString.Internal (packChars)
import Data.Conduit.List
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Test.Framework hiding (Success)

import Chiasma.Native.StreamParse (TmuxOutputBlock(Success), parseConduit)

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

test_conduit :: IO ()
test_conduit = do
  r <- runConduit $ sourceList tmuxOutput .| mapC (packChars . T.unpack) .| parseConduit .| sinkList
  assertEqual [Success [], Success [paneLine, "b"], Success ["c", "d"]] r
