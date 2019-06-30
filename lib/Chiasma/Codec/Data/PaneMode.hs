{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data.PaneMode where

import GHC.Generics (Generic)

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId(paneId))

data PaneMode =
  PaneMode {
    paneId :: PaneId,
    paneMode :: String
  }
  deriving (Eq, Show, Generic, TmuxCodec)

instance HasPaneId PaneMode where
  paneId = paneId
