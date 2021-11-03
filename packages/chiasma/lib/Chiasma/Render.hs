module Chiasma.Render where

import Path (Abs, Dir, Path)
import Prettyprinter (line, pretty)

import Chiasma.Codec.Data.Pane (Pane)
import qualified Chiasma.Codec.Data.Window as Codec (Window (Window))
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Panes (TmuxPanes)
import qualified Chiasma.Data.RenderError as RenderError
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Pack (packWindow)
import Chiasma.Session (ensureSession, findOrCreateSession)
import Chiasma.Ui.Data.RenderableTree (RenderableTree)
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.View (viewsLog)
import Chiasma.Window (ensureView, ensureWindow, findOrCreateWindow, windowState)

renderTree ::
  Members [TmuxPanes Pane, AtomicState Views, Tmux] r =>
  Ident ->
  Codec.Window ->
  RenderableTree ->
  Sem r ()
renderTree windowIdent window tree = do
  viewsLog $ pretty ("rendering tree in window " <> identText windowIdent <> ":") <> line <> pretty tree
  wState <- windowState windowIdent window tree
  packWindow wState

render ::
  Members [TmuxPanes Pane !! CodecError, AtomicState Views, Tmux !! CodecError, Stop RenderError] r =>
  Path Abs Dir ->
  Ident ->
  Ident ->
  ViewTree ->
  Sem r ()
render cwd sessionIdent windowIdent tree = do
  resumeHoist @_ @(TmuxPanes _) RenderError.Codec do
    resumeHoist @_ @Tmux RenderError.Codec do
      initialSession <- findOrCreateSession sessionIdent
      initialWindow <- findOrCreateWindow windowIdent
      (sid, newSessionWid) <- mapStop RenderError.Layout (ensureSession initialSession initialWindow)
      window@(Codec.Window windowId _ _) <- ensureWindow sid initialWindow newSessionWid tree
      traverse_ (renderTree windowIdent window) =<< ensureView cwd windowId tree
