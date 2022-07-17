module Chiasma.View where

import qualified Control.Lens as Lens
import Exon (exon)
import Prettyprinter (Doc, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Chiasma.Data.Ident (Ident, identText, sameIdent)
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.View (View (View), viewIdent)
import Chiasma.Data.Views (Views, ViewsError (..))
import qualified Chiasma.Data.Views as Views (log, panes, sessions, windows)
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
insertView viewsL newView = over viewsL (newView :)

updateView :: Lens' Views [View a] -> (Ident -> ViewsError) -> View a -> Views -> Views
updateView viewsL _ newView =
  set (viewsL . where1 (sameIdent (viewIdent newView))) newView

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

viewsLog ::
  Member (AtomicState Views) r =>
  Doc AnsiStyle ->
  Sem r ()
viewsLog message =
  atomicModify' f
  where
    f :: Views -> Views
    f = over Views.log (message :)

viewsLogS ::
  Member (AtomicState Views) r =>
  Text ->
  Sem r ()
viewsLogS =
  viewsLog . pretty

addView ::
  Member (AtomicState Views) r =>
  Setter a ->
  Ident ->
  Sem r (View a)
addView setter ident = do
  atomicModify' (setter newView)
  viewsLogS [exon|added tmux view #{identText ident}|]
  pure newView
  where
    newView =
      View ident Nothing

findOrCreateView ::
  Member (AtomicState Views) r =>
  Getter a ->
  Setter a ->
  Ident ->
  Sem r (View a)
findOrCreateView getter setter ident = do
  existing <- atomicGets (getter ident)
  either (const (addView setter ident)) pure existing
