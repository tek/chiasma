module Chiasma.Data.WindowState(
  WindowState(..),
) where

import qualified Chiasma.Codec.Data as Codec (Window, Pane)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Ui.Data.RenderableTree (RenderableTree)

data WindowState =
  WindowState {
    wsNativeWindow :: Codec.Window,
    wsNativeRefPane :: Codec.Pane,
    wsWindowIdent :: Ident,
    wsLayout :: RenderableTree,
    wsReferencePane :: PaneId
  }
  deriving (Eq, Show)
