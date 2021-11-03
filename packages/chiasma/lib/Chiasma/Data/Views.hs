module Chiasma.Data.Views where

import Control.Lens (makeClassy)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.View (View)

data ViewsError =
  NoSuchSession Ident
  |
  NoSuchWindow Ident
  |
  NoSuchPane Ident
  |
  NoPaneId Ident
  deriving stock (Eq, Show)

data Views =
  Views {
    _sessions :: [View SessionId],
    _windows :: [View WindowId],
    _panes :: [View PaneId],
    _log :: [Doc AnsiStyle]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)

makeClassy ''Views

instance Eq Views where
  (Views sa wa pa _) == (Views sb wb pb _) =
    (sa == sb) && (wa == wb) && (pa == pb)
