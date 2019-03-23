{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Codec.Data.Window where

import GHC.Generics (Generic)

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (WindowId)

data Window =
  Window {
    windowId :: WindowId,
    windowWidth :: Int,
    windowHeight :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)
