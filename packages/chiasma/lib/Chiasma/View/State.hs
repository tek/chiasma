module Chiasma.View.State where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.View as Views (paneId)

paneId ::
  Members [AtomicState Views, Stop ViewsError] r =>
  Ident ->
  Sem r PaneId
paneId paneIdent =
  stopEither =<< atomicGets (Views.paneId paneIdent)
