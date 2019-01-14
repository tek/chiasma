module Chiasma.Data.WindowState(
  WindowStateType(..),
  WindowState(..),
) where

import Chiasma.Data.Ident (Ident)
import qualified Chiasma.Codec.Data as Codec (Window, Pane)
import qualified Chiasma.Data.View as Tmux (View)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Ui.Data.View (ViewTree)

data WindowStateType =
  Pristine
  |
  Tracked {
    wstReferencePane :: Tmux.View PaneId
  }
  deriving (Eq, Show)

data WindowState =
  WindowState {
    wsNativeWindow :: Codec.Window,
    wsNativeRefPane :: Codec.Pane,
    wsWindowIdent :: Ident,
    wsLayout :: ViewTree,
    wsType :: WindowStateType
  }
  deriving (Eq, Show)
