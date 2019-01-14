{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chiasma.Render(
  render,
) where

import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Data.Either.Combinators (mapLeft)
import qualified Chiasma.Codec.Data as Codec (Window(Window))
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Pack (packWindow)
import Chiasma.Session (findOrCreateSession, ensureSession)
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Window (findOrCreateWindow, ensureWindow, principalPane, ensureView, windowState)

coalesceError :: Functor m => (e -> RenderError) -> ExceptT e m a -> ExceptT RenderError m a
coalesceError cons = mapExceptT (fmap $ mapLeft cons)

render ::
  (MonadState Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  Ident ->
  Ident ->
  ViewTree ->
  ExceptT RenderError m ()
render cwd sessionIdent windowIdent tree = do
  initialSession <- findOrCreateSession sessionIdent
  initialWindow <- findOrCreateWindow windowIdent
  sid <- ensureSession initialSession
  window@(Codec.Window windowId _ _) <- ensureWindow sid initialWindow tree
  ensureView cwd windowId tree
  (uiPrinc, _) <- principalPane tree
  wState <- windowState windowIdent window tree
  coalesceError RenderError.Pack $ packWindow wState windowId uiPrinc
