{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Render(
  render,
) where

import Control.Monad (when)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Data.Either.Combinators (mapLeft)
import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Maybe (orElse)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Lens.Tree (LeafIndexTree)
import Chiasma.Pack (packWindow)
import Chiasma.Session (findOrCreateSession, ensureSession)
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (hasOpenPanes)
import Chiasma.Window (findOrCreateWindow, ensureWindow, principalPane, ensureView, windowState, registerWindowId)

import Chiasma.Command.Pane (panes)
import Chiasma.Command.Window (windows)
import Chiasma.Ui.ShowTree (printViewTree)

coalesceError :: Functor m => (e -> RenderError) -> ExceptT e m a -> ExceptT RenderError m a
coalesceError cons = mapExceptT (fmap $ mapLeft cons)

unsafeRender ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  Ident ->
  Ident ->
  ViewTree ->
  ExceptT RenderError m ()
unsafeRender cwd sessionIdent windowIdent tree = do
  initialSession <- findOrCreateSession sessionIdent
  initialWindow <- findOrCreateWindow windowIdent
  (sid, newSessionWid) <- ensureSession initialSession
  window@(Codec.Window windowId _ _) <- ensureWindow sid initialWindow newSessionWid tree
  ensureView cwd windowId tree
  (uiPrinc, _) <- principalPane tree
  wState <- windowState windowIdent window tree
  coalesceError RenderError.Pack $ packWindow wState windowId uiPrinc

render ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  Ident ->
  Ident ->
  ViewTree ->
  ExceptT RenderError m ()
render cwd sessionIdent windowIdent tree =
  when (hasOpenPanes tree) $ unsafeRender cwd sessionIdent windowIdent tree
