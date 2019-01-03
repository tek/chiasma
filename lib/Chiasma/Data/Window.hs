module Chiasma.Data.Window(
  Window(..),
  WindowId(..),
) where

import Chiasma.Codec.Decode (TmuxPrimDecode(..), parseId)

newtype WindowId =
  WindowId Int
  deriving (Eq, Show)

instance TmuxPrimDecode WindowId where
  primDecode = parseId WindowId '@'

data Window =
  Window Int Int Int
