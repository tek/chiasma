module Chiasma.Ui.Pane(
  paneToggleOpen,
  paneSetOpen,
) where

import Chiasma.Ui.Data.View (View(View), PaneView, Pane(Pane))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))

paneToggleOpen :: PaneView -> PaneView
paneToggleOpen (View i s g (Pane False pin cwd)) =
  View i s g (Pane True pin cwd)
paneToggleOpen (View i (ViewState m) g e) =
  View i (ViewState (not m)) g e

paneSetOpen :: PaneView -> PaneView
paneSetOpen (View i s g (Pane _ pin cwd)) =
  View i s g (Pane True pin cwd)
