{-# OPTIONS_GHC -F -pgmF htfpp #-}

module CodecSpec(
  htf_thisModulesTests,
) where
import GHC.Generics (Generic)
import Test.Framework
import Chiasma.Data.TmuxId (PaneId(..), WindowId(..))
import Chiasma.Codec (TmuxCodec(decode, query), TmuxQuery(TmuxQuery))

data Dat =
  Dat {
    paneId :: PaneId,
    windowId :: WindowId
  }
  deriving (Eq, Show, Generic)

instance TmuxCodec Dat

test_decode :: IO ()
test_decode =
  assertEqual (Right $ Dat (PaneId 1) (WindowId 2)) $ decode " %1 @2 "

test_query :: IO ()
test_query =
  assertEqual (TmuxQuery "#{pane_id} #{window_id}") (query @Dat)
