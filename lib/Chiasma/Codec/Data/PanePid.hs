{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data.PanePid where

import GHC.Generics (Generic)

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId(paneId))

data PanePid =
  PanePid {
    paneId :: PaneId,
    panePid :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)

instance HasPaneId PanePid where
  paneId = paneId
