{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Render(
  render,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Data.Foldable (forM_)
import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (pretty, line, (<>))

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Pack (packWindow)
import Chiasma.Session (findOrCreateSession, ensureSession)
import Chiasma.Ui.Data.RenderableTree (RenderableTree)
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (hasOpenPanes)
import Chiasma.View (viewsLog)
import Chiasma.Window (findOrCreateWindow, ensureWindow, principalPane, ensureView, windowState)

renderTree ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Ident ->
  Codec.Window ->
  RenderableTree ->
  m ()
renderTree windowIdent window tree = do
  viewsLog $ pretty (T.pack $ "rendering tree in window " ++ identString windowIdent ++ ":") <> line <> pretty tree
  wState <- windowState windowIdent window tree
  packWindow wState

render ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Ident ->
  Ident ->
  ViewTree ->
  m ()
render cwd sessionIdent windowIdent tree = do
  initialSession <- findOrCreateSession sessionIdent
  initialWindow <- findOrCreateWindow windowIdent
  (sid, newSessionWid) <- ensureSession initialSession initialWindow
  window@(Codec.Window windowId _ _) <- ensureWindow sid initialWindow newSessionWid tree
  renderableTree <- ensureView cwd windowId tree
  forM_ renderableTree $ renderTree windowIdent window
