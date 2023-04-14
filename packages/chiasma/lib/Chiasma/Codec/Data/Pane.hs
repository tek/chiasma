module Chiasma.Codec.Data.Pane where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.TmuxId (HasPaneId, PaneId)
import qualified Chiasma.Data.TmuxId as HasPaneId (HasPaneId (..))

data Pane =
  Pane {
    paneId :: PaneId,
    paneWidth :: Int,
    paneHeight :: Int,
    paneTop :: Int,
    paneLeft :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)

instance HasPaneId Pane where
  paneId = (.paneId)
