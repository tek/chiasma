module Chiasma.Render where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Data.Foldable (forM_)
import Data.Text.Prettyprint.Doc (line, pretty, (<>))

import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Pack (packWindow)
import Chiasma.Session (ensureSession, findOrCreateSession)
import Chiasma.Ui.Data.RenderableTree (RenderableTree)
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.View (viewsLog)
import Chiasma.Window (ensureView, ensureWindow, findOrCreateWindow, windowState)

renderTree ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  Ident ->
  Codec.Window ->
  RenderableTree ->
  m ()
renderTree windowIdent window tree = do
  viewsLog $ pretty ("rendering tree in window " <> identText windowIdent <> ":") <> line <> pretty tree
  wState <- windowState windowIdent window tree
  packWindow wState

render ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
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
