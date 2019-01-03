{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CodecSpec(
  htf_thisModulesTests,
) where
import GHC.Generics (Generic)
import Test.Framework
import Chiasma.Data.Pane (PaneId(..))
import Chiasma.Data.Window (WindowId(..))
import Chiasma.Codec (TmuxCodec(decode))
import Chiasma.Codec.Decode (TmuxDecodeError)

data Dat =
  Dat {
    paneId :: PaneId,
    windowId :: WindowId
  }
  deriving (Eq, Show, Generic, TmuxCodec)

decodeTest :: [String] -> Either TmuxDecodeError Dat
decodeTest = decode

test_codec :: IO ()
test_codec =
  assertEqual (Right $ Dat (PaneId 1) (WindowId 2)) $ decodeTest ["%1", "@2"]
