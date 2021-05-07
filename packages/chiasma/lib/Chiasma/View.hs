{-# LANGUAGE RankNTypes #-}

module Chiasma.View where

import Chiasma.Data.Ident (Ident, identText, sameIdent)
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.View (View(View), viewIdent)
import Chiasma.Data.Views (Views, ViewsError(..))
import qualified Chiasma.Data.Views as Views (log, panes, sessions, windows)
import Chiasma.Lens.Where (where1)
import Control.Lens (Lens', over)
import qualified Control.Lens as Lens (over, set, view)
import Data.Text.Prettyprint.Doc (Doc, pretty)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

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
session = view Views.sessions NoSuchSession

sessionById :: SessionId -> Views -> Maybe (View SessionId)
sessionById = viewById Views.sessions

insertSession :: View SessionId -> Views -> Views
insertSession = insertView Views.sessions

updateSession :: View SessionId -> Views -> Views
updateSession = updateView Views.sessions NoSuchSession

window :: Ident -> Views -> Either ViewsError (View WindowId)
window = view Views.windows NoSuchWindow

windowById :: WindowId -> Views -> Maybe (View WindowId)
windowById = viewById Views.windows

insertWindow :: View WindowId -> Views -> Views
insertWindow = insertView Views.windows

updateWindow :: View WindowId -> Views -> Views
updateWindow = updateView Views.windows NoSuchWindow

pane :: Ident -> Views -> Either ViewsError (View PaneId)
pane = view Views.panes NoSuchPane

paneById :: PaneId -> Views -> Maybe (View PaneId)
paneById = viewById Views.panes

paneId :: Ident -> Views -> Either ViewsError PaneId
paneId paneIdent views =
  pane paneIdent views >>= trans
  where
    trans (View _ (Just paneId')) = Right paneId'
    trans _ = Left $ NoPaneId paneIdent

insertPane :: View PaneId -> Views -> Views
insertPane = insertView Views.panes

updatePane :: View PaneId -> Views -> Views
updatePane = updateView Views.panes NoSuchPane

type Getter a = Ident -> Views -> Either ViewsError (View a)
type Setter a = View a -> Views -> Views

addView :: MonadDeepState s Views m => Setter a -> Ident -> m (View a)
addView setter ident = do
  modify $ setter newView
  viewsLogS $ "added tmux view " <> identText ident
  return newView
  where
    newView = View ident Nothing

findOrCreateView :: (MonadDeepState s Views m) => Getter a -> Setter a -> Ident -> m (View a)
findOrCreateView getter setter ident = do
  existing <- gets $ getter ident
  either (const $ addView setter ident) return existing

viewsLog :: MonadDeepState s Views m => Doc AnsiStyle -> m ()
viewsLog message =
  modify f
  where
    f :: Views -> Views
    f = over Views.log (message :)

viewsLogS :: MonadDeepState s Views m => Text -> m ()
viewsLogS =
  viewsLog . pretty
