{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data.PaneCoords where

import GHC.Generics (Generic)

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId, SessionId, WindowId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId(..))

data PaneCoords =
  PaneCoords {
    sessionId :: SessionId,
    windowId :: WindowId,
    paneId :: PaneId
  }
  deriving (Eq, Show, Generic, TmuxCodec)

instance HasPaneId PaneCoords where
  paneId = paneId
