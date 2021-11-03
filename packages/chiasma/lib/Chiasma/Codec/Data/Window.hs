module Chiasma.Codec.Data.Window where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (WindowId)

data Window =
  Window {
    windowId :: WindowId,
    windowWidth :: Int,
    windowHeight :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)
