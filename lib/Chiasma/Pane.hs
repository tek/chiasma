module Chiasma.Pane(
  addPane,
) where

import Control.Monad.State.Class (MonadState, modify)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views)
import qualified Chiasma.Data.View as Tmux (View(View))
import qualified Chiasma.View as Views (insertPane)

addPane :: MonadState Views m => Ident -> m (Tmux.View PaneId)
addPane ident = do
  modify $ Views.insertPane pane
  return pane
  where
    pane = Tmux.View ident Nothing
