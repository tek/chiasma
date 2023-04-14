module Chiasma.Codec.Data.PanePid where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId (paneId))

data PanePid =
  PanePid {
    paneId :: PaneId,
    panePid :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)

instance HasPaneId PanePid where
  paneId = (.paneId)
