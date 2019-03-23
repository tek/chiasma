module Chiasma.View.State(
  paneId,
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.DeepState (MonadDeepState, gets)

import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.View as Views (paneId)

paneId ::
  (MonadDeepState s Views m, MonadDeepError e ViewsError m) =>
  Ident ->
  m PaneId
paneId paneIdent = do
  pid <- gets $ Views.paneId paneIdent
  hoistEither pid
