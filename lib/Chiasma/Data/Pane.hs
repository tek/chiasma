module Chiasma.Data.Pane(
  Pane(..),
  PaneId(..),
) where

import GHC.Generics (Generic)
import Chiasma.Codec (TmuxCodec)
import Chiasma.Codec.Decode (TmuxPrimDecode(..), parseId)

newtype PaneId =
  PaneId Int
  deriving (Eq, Show)

instance TmuxPrimDecode PaneId where
  primDecode = parseId PaneId '%'

data Pane =
  Pane {
    paneId :: PaneId,
    paneWidth :: Int,
    paneHeight :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)
