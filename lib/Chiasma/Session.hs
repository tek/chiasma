{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Session where

import qualified Chiasma.Codec.Data as Codec (Session(Session), Window(Window))
import Chiasma.Command.Session (existingSessionId, newSession)
import Chiasma.Command.Window (newSessionWindow)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (SessionId, WindowId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Data.View as Tmux (
  View(viewId, viewIdent),
  setViewId,
  )
import Chiasma.Data.Views (Views)
import Chiasma.View (findOrCreateView, viewsLogS)
import qualified Chiasma.View as Views (insertSession, session, updateSession, updateWindow)
import Control.Monad (join)
import Control.Monad.DeepState (MonadDeepState, modify)
import Control.Monad.Free.Class (MonadFree)
import Data.Bifunctor (second)

findOrCreateSession ::
  MonadDeepState s Views m =>
  Ident ->
  m (Tmux.View SessionId)
findOrCreateSession =
  findOrCreateView Views.session Views.insertSession

spawnSession ::
  MonadDeepState s Views m =>
  MonadFree TmuxThunk m =>
  Tmux.View SessionId ->
  Tmux.View WindowId ->
  m (SessionId, WindowId)
spawnSession session' window = do
  Codec.Session sid <- newSession (Tmux.viewIdent session')
  modify $ Views.updateSession $ Tmux.setViewId sid session'
  Codec.Window wid _ _ <- newSessionWindow sid
  modify $ Views.updateWindow $ Tmux.setViewId wid window
  viewsLogS $ "spawned session " ++ show session' ++ " with id " ++ show sid ++ " and window id " ++ show wid
  return (sid, wid)

ensureSession ::
  MonadDeepState s Views m =>
  MonadFree TmuxThunk m =>
  Tmux.View SessionId ->
  Tmux.View WindowId ->
  m (SessionId, Maybe WindowId)
ensureSession session' window = do
  existing <- join <$> traverse existingSessionId (Tmux.viewId session')
  case existing of
    Just sid -> return (sid, Nothing)
    Nothing -> second Just <$> spawnSession session' window
