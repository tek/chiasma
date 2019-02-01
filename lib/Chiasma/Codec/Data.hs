{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data(
  Session(..),
  Window(..),
  Pane(..),
) where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (SessionId, WindowId, PaneId)
import GHC.Generics (Generic)

data Session =
  Session {
    sessionId :: SessionId
  }
  deriving (Eq, Show, Generic, TmuxCodec)

data Window =
  Window {
    windowId :: WindowId,
    windowWidth :: Int,
    windowHeight :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)

data Pane =
  Pane {
    paneId :: PaneId,
    paneWidth :: Int,
    paneHeight :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)

