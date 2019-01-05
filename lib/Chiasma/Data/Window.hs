module Chiasma.Data.Window(
  Window(..),
  WindowId(..),
) where

import GHC.Generics (Generic)
import Chiasma.Codec (TmuxCodec)
import Chiasma.Codec.Decode (TmuxPrimDecode(..), parseId)

newtype WindowId =
  WindowId Int
  deriving (Eq, Show)

instance TmuxPrimDecode WindowId where
  primDecode = parseId WindowId '@'

data Window =
  Window {
    windowId :: WindowId,
    windowWidth :: Int,
    windowHeight :: Int
  }
  deriving (Eq, Show, Generic, TmuxCodec)
