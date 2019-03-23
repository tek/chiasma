{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data.PanePid where

import GHC.Generics (Generic)

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (PaneId)

data PanePid =
  PanePid {
    paneId :: PaneId,
    panePid :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)
