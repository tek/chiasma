module Chiasma.Codec.Data.PaneCoords where

import Chiasma.Data.TmuxId (HasPaneId, PaneId, SessionId, WindowId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId(..))

import Chiasma.Codec (TmuxCodec)

data PaneCoords =
  PaneCoords {
    sessionId :: SessionId,
    windowId :: WindowId,
    paneId :: PaneId
  }
  deriving (Eq, Show, Generic, TmuxCodec)

instance HasPaneId PaneCoords where
  paneId = paneId
