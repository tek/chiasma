module Chiasma.Test.CodecTest where

import Chiasma.Codec (TmuxCodec(decode, query), TmuxQuery(TmuxQuery))
import Chiasma.Data.TmuxId (PaneId(..), WindowId(..))
import Hedgehog ((===))

import Chiasma.Test.Util (UnitTest)

data Dat =
  Dat {
    paneId :: PaneId,
    windowId :: WindowId
  }
  deriving (Eq, Show, Generic)

instance TmuxCodec Dat

test_decode :: UnitTest
test_decode =
  Right (Dat (PaneId 1) (WindowId 2)) === decode " %1 @2 "

test_query :: UnitTest
test_query =
  TmuxQuery "#{pane_id} #{window_id}" === query @Dat
