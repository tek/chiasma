module Chiasma.Ui.Pane(
  paneToggleOpen,
) where

import Chiasma.Ui.Data.View (View(..), PaneView, Pane(..))
import Chiasma.Ui.Data.ViewState (ViewState(..))

paneToggleOpen :: PaneView -> PaneView
paneToggleOpen (View i s g (Pane False pin cwd)) =
  View i s g (Pane True pin cwd)
paneToggleOpen (View i (ViewState m) g e) =
  View i (ViewState (not m)) g e
