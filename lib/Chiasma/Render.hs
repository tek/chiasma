{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Render(
  render,
) where

import Control.Monad (when)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Error.Class (MonadError)
import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Pack (packWindow)
import Chiasma.Session (findOrCreateSession, ensureSession)
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (hasOpenPanes)
import Chiasma.Window (findOrCreateWindow, ensureWindow, principalPane, ensureView, windowState)

unsafeRender ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Ident ->
  Ident ->
  ViewTree ->
  m ()
unsafeRender cwd sessionIdent windowIdent tree = do
  initialSession <- findOrCreateSession sessionIdent
  initialWindow <- findOrCreateWindow windowIdent
  (sid, newSessionWid) <- ensureSession initialSession initialWindow
  window@(Codec.Window windowId _ _) <- ensureWindow sid initialWindow newSessionWid tree
  ensureView cwd windowId tree
  (uiPrinc, _) <- principalPane tree
  wState <- windowState windowIdent window tree
  packWindow wState windowId uiPrinc

render ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Ident ->
  Ident ->
  ViewTree ->
  m ()
render cwd sessionIdent windowIdent tree =
  when (hasOpenPanes tree) $ unsafeRender cwd sessionIdent windowIdent tree
