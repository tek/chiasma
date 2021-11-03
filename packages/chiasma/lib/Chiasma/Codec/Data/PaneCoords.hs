module Chiasma.Codec.Data.PaneCoords where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId, SessionId, WindowId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId (..))

data PaneCoords =
  PaneCoords {
    sessionId :: SessionId,
    windowId :: WindowId,
    paneId :: PaneId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)

instance HasPaneId PaneCoords where
  paneId = paneId
