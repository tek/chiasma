module Chiasma.Pane where

import Control.Monad.DeepState (MonadDeepState, modify)

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views)
import qualified Chiasma.View as Views (insertPane)

addPane :: MonadDeepState s Views m => Ident -> m (Tmux.View PaneId)
addPane ident = do
  modify $ Views.insertPane pane
  return pane
  where
    pane = Tmux.View ident Nothing
