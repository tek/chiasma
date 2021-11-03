module Chiasma.Test.CodecTest where

import Hedgehog ((===))

import Chiasma.Codec (TmuxCodec (decode, query))
import Chiasma.Data.TmuxId (PaneId (..), WindowId (..))
import Chiasma.Data.TmuxQuery (TmuxQuery (TmuxQuery))
import Chiasma.Test.Util (UnitTest)

data Dat =
  Dat {
    paneId :: PaneId,
    windowId :: WindowId
  }
  deriving stock (Eq, Show, Generic)

instance TmuxCodec Dat

test_decode :: UnitTest
test_decode =
  Right (Dat (PaneId 1) (WindowId 2)) === decode " %1 @2 "

test_query :: UnitTest
test_query =
  TmuxQuery "#{pane_id} #{window_id}" === query @Dat
