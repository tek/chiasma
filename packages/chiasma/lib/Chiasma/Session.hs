module Chiasma.Session where

import Exon (exon)
import qualified Chiasma.Codec.Data.Session as Codec (Session (Session))
import qualified Chiasma.Codec.Data.Window as Codec (Window (Window))
import Chiasma.Command.Session (existingSessionId, newSession)
import Chiasma.Command.Window (newSessionWindow)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.LayoutError (LayoutError)
import Chiasma.Data.TmuxId (SessionId, WindowId)
import qualified Chiasma.Data.View as Tmux (View (viewId, viewIdent), setViewId)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.View (findOrCreateView, viewsLogS)
import qualified Chiasma.View as Views (insertSession, session, updateSession, updateWindow)

findOrCreateSession ::
  Member (AtomicState Views) r =>
  Ident ->
  Sem r (Tmux.View SessionId)
findOrCreateSession =
  findOrCreateView Views.session Views.insertSession

spawnSession ::
  Members [AtomicState Views, Tmux, Stop LayoutError] r =>
  Tmux.View SessionId ->
  Tmux.View WindowId ->
  Sem r (SessionId, WindowId)
spawnSession session' window = do
  Codec.Session sid _ <- newSession (Tmux.viewIdent session')
  atomicModify' (Views.updateSession (Tmux.setViewId sid session'))
  Codec.Window wid _ _ <- newSessionWindow sid
  atomicModify' (Views.updateWindow (Tmux.setViewId wid window))
  viewsLogS [exon|spawned session #{show session'} with id #{show sid} and window id #{show wid}|]
  pure (sid, wid)

ensureSession ::
  Members [AtomicState Views, Tmux, Stop LayoutError] r =>
  Tmux.View SessionId ->
  Tmux.View WindowId ->
  Sem r (SessionId, Maybe WindowId)
ensureSession session' window = do
  existing <- join <$> traverse existingSessionId (Tmux.viewId session')
  dbgs existing
  case existing of
    Just sid -> pure (sid, Nothing)
    Nothing -> second Just <$> spawnSession session' window
