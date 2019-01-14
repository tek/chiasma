{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Session(
  findOrCreateSession,
  ensureSession,
) where

import Control.Monad (join)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.Trans.Except (ExceptT)
import Chiasma.Command.Session (newSession, existingSessionId)
import qualified Chiasma.Codec.Data as Codec (Session(Session))
import Chiasma.Data.TmuxId (SessionId)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Data.View as Tmux (
  View(viewId, viewIdent),
  setViewId,
  )
import Chiasma.Data.Views (Views)
import Chiasma.View (findOrCreateView)
import qualified Chiasma.View as Views (session, insertSession, updateSession)

findOrCreateSession :: MonadState Views m => Ident -> ExceptT RenderError m (Tmux.View SessionId)
findOrCreateSession = findOrCreateView Views.session Views.insertSession

spawnSession ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  Tmux.View SessionId ->
  m SessionId
spawnSession session' = do
  Codec.Session sid <- newSession (Tmux.viewIdent session')
  modify $ Views.updateSession $ Tmux.setViewId sid session'
  return sid

ensureSession ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  Tmux.View SessionId ->
  m SessionId
ensureSession session' = do
  existing <- join <$> traverse existingSessionId (Tmux.viewId session')
  maybe (spawnSession session') return existing
