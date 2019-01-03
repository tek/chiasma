module Chiasma.Data.Pane(
  Pane(..),
  PaneId(..),
) where

import Chiasma.Codec.Decode (TmuxPrimDecode(..), parseId)

newtype PaneId =
  PaneId Int
  deriving (Eq, Show)

instance TmuxPrimDecode PaneId where
  primDecode = parseId PaneId '%'

data Pane =
  Pane PaneId Int Int
