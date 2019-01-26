{-# LANGUAGE RankNTypes #-}

module Chiasma.View(
  findOrCreateView,
  view,
  insertView,
  updateView,
  session,
  insertSession,
  updateSession,
  window,
  insertWindow,
  updateWindow,
  pane,
  insertPane,
  updatePane,
  sameId,
  viewById,
  sessionById,
  windowById,
  paneById,
) where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (view, over, set)
import Control.Monad.State.Class (MonadState, modify, gets)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (find)
import Chiasma.Data.TmuxId (SessionId, WindowId, PaneId)
import Chiasma.Data.Ident (Ident, sameIdent)
import Chiasma.Data.View (View(View), viewIdent)
import Chiasma.Data.Views (Views, ViewsError(..), _viewsSessions, _viewsWindows, _viewsPanes)
import Chiasma.Lens.Where (where1)

sameId :: Eq a => a -> View a -> Bool
sameId id' (View _ (Just vid)) = id' == vid
sameId _ _ = False

view :: Lens' Views [View a] -> (Ident -> ViewsError) -> Ident -> Views -> Either ViewsError (View a)
view viewsL consError ident =
  maybeToRight (consError ident) . find (sameIdent ident) . Lens.view viewsL

viewById :: Eq a => Lens' Views [View a] -> a -> Views -> Maybe (View a)
viewById viewsL id' =
  find (sameId id') . Lens.view viewsL

insertView :: Lens' Views [View a] -> View a -> Views -> Views
insertView viewsL newView = Lens.over viewsL (newView :)

updateView :: Lens' Views [View a] -> (Ident -> ViewsError) -> View a -> Views -> Views
updateView viewsL _ newView =
  Lens.set (viewsL . where1 (sameIdent (viewIdent newView))) newView

session :: Ident -> Views -> Either ViewsError (View SessionId)
session = view _viewsSessions NoSuchSession

sessionById :: SessionId -> Views -> Maybe (View SessionId)
sessionById = viewById _viewsSessions

insertSession :: View SessionId -> Views -> Views
insertSession = insertView _viewsSessions

updateSession :: View SessionId -> Views -> Views
updateSession = updateView _viewsSessions NoSuchSession

window :: Ident -> Views -> Either ViewsError (View WindowId)
window = view _viewsWindows NoSuchWindow

windowById :: WindowId -> Views -> Maybe (View WindowId)
windowById = viewById _viewsWindows

insertWindow :: View WindowId -> Views -> Views
insertWindow = insertView _viewsWindows

updateWindow :: View WindowId -> Views -> Views
updateWindow = updateView _viewsWindows NoSuchWindow

pane :: Ident -> Views -> Either ViewsError (View PaneId)
pane = view _viewsPanes NoSuchPane

paneById :: PaneId -> Views -> Maybe (View PaneId)
paneById = viewById _viewsPanes

insertPane :: View PaneId -> Views -> Views
insertPane = insertView _viewsPanes

updatePane :: View PaneId -> Views -> Views
updatePane = updateView _viewsPanes NoSuchPane

type Getter a = Ident -> Views -> Either ViewsError (View a)
type Setter a = View a -> Views -> Views

addView :: MonadState Views m => Setter a -> Ident -> m (View a)
addView setter ident = do
  modify $ setter newView
  return newView
  where
    newView = View ident Nothing

findOrCreateView :: MonadState Views m => Getter a -> Setter a -> Ident -> m (View a)
findOrCreateView getter setter ident = do
  existing <- gets $ getter ident
  either (const $ addView setter ident) return existing