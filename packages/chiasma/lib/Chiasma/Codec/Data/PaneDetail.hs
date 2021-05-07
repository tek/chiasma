module Chiasma.Codec.Data.PaneDetail where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId(..))

data PaneDetail =
  PaneDetail {
    paneId :: PaneId,
    paneWidth :: Int,
    paneHeight :: Int,
    paneTop :: Int,
    paneLeft :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)

instance HasPaneId PaneDetail where
  paneId = paneId
