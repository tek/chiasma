module Chiasma.View.State(
  paneId,
) where

import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Ident (Ident)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.Error.Class (MonadError, liftEither)

import qualified Chiasma.View as Views (paneId)
import Chiasma.Data.Views (Views, ViewsError)

paneId ::
  (MonadState Views m, MonadError ViewsError m) =>
  Ident ->
  m PaneId
paneId paneIdent = do
  pid <- gets $ Views.paneId paneIdent
  liftEither pid
