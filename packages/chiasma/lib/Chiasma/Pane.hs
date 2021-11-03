module Chiasma.Pane where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import qualified Chiasma.Data.View as Tmux (View (View))
import Chiasma.Data.Views (Views)
import qualified Chiasma.View as Views (insertPane)

addPane ::
  Member (AtomicState Views) r =>
  Ident ->
  Sem r (Tmux.View PaneId)
addPane ident =
  pane <$ atomicModify' (Views.insertPane pane)
  where
    pane =
      Tmux.View ident Nothing
