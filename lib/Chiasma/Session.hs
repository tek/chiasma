{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Session(
  findOrCreateSession,
  ensureSession,
) where

import Control.Monad (join)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState, modify)
import Data.Bifunctor (second)
import Chiasma.Command.Session (newSession, existingSessionId)
import Chiasma.Command.Window (newSessionWindow)
import qualified Chiasma.Codec.Data as Codec (Session(Session), Window(Window))
import Chiasma.Data.TmuxId (SessionId, WindowId)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Data.View as Tmux (
  View(viewId, viewIdent),
  setViewId,
  )
import Chiasma.Data.Views (Views)
import Chiasma.View (findOrCreateView)
import qualified Chiasma.View as Views (session, insertSession, updateSession)

findOrCreateSession :: MonadState Views m => Ident -> m (Tmux.View SessionId)
findOrCreateSession = findOrCreateView Views.session Views.insertSession

spawnSession ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  Tmux.View SessionId ->
  m (SessionId, WindowId)
spawnSession session' = do
  Codec.Session sid <- newSession (Tmux.viewIdent session')
  modify $ Views.updateSession $ Tmux.setViewId sid session'
  Codec.Window wid _ _ <- newSessionWindow sid
  return (sid, wid)

ensureSession ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  Tmux.View SessionId ->
  m (SessionId, Maybe WindowId)
ensureSession session' = do
  existing <- join <$> traverse existingSessionId (Tmux.viewId session')
  case existing of
    Just sid -> return (sid, Nothing)
    Nothing -> second Just <$> spawnSession session'
