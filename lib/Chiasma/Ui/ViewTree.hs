module Chiasma.Ui.ViewTree where

import Control.Lens (anyOf, cosmos, filtered, has, ix, mapMOf, transformM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Bifunctor (bimap, first, second)
import Data.Composition ((.:))
import Data.Foldable (fold)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Sum(..))
import Data.Text.Prettyprint.Doc (pretty)
import Data.Traversable (mapAccumL)

import Chiasma.Control.IO.Unsafe (unsafeLog)
import Chiasma.Data.Ident (Ident)
import Chiasma.Lens.Tree (
  LeafIndexTree(..),
  _litTree,
  leafDataTraversal,
  )
import Chiasma.Ui.Data.TreeModError (TreeModError(PaneMissing, AmbiguousPane, LayoutMissing, AmbiguousLayout))
import Chiasma.Ui.Data.View (
  Pane(Pane),
  PaneView,
  Tree(Tree),
  TreeSub(TreeNode, TreeLeaf),
  View(View),
  ViewTree,
  ViewTreeSub,
  )
import qualified Chiasma.Ui.Data.View as Pane (open)
import qualified Chiasma.Ui.Data.View as TreeSub (leafData)
import qualified Chiasma.Ui.Data.View as View (extra)
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.Pane (paneSetOpen, paneToggleOpen)

modCounted :: Monad m => (a -> m a) -> a -> WriterT (Sum Int) m a
modCounted f a = do
  tell (Sum 1)
  lift $ f a

treeToggleOpen :: ViewTree -> ViewTree
treeToggleOpen (Tree l sub) =
  Tree l (snd $ mapAccumL toggle False sub)
  where
    toggle False (TreeLeaf p) = (True, TreeLeaf (paneToggleOpen p))
    toggle a b = (a, b)

modifyTreeUniqueM :: Monad m => (ViewTree -> m ViewTree) -> Ident -> ViewTree -> ExceptT TreeModError m ViewTree
modifyTreeUniqueM f ident tree = do
  let st = (transformM $ mapMOf (ix ident) (modCounted f)) tree
  (result, Sum count) <- lift $ runWriterT st
  case count of
    1 -> return result
    0 -> throwError $ LayoutMissing ident
    n -> throwError $ AmbiguousLayout ident n

toggleLayout1 :: Ident -> ViewTree -> Either TreeModError ViewTree
toggleLayout1 ident tree =
  runIdentity $ runExceptT $ modifyTreeUniqueM (Identity . treeToggleOpen) ident tree

modifyPaneUniqueM :: Monad m => (PaneView -> m PaneView) -> Ident -> ViewTree -> ExceptT TreeModError m ViewTree
modifyPaneUniqueM f ident tree = do
  let st = (transformM $ mapMOf (ix ident) (modCounted f)) (LeafIndexTree tree)
  (result, Sum count) <- lift $ runWriterT st
  case count of
    1 -> return $ litTree result
    0 -> throwError $ PaneMissing ident
    n -> throwError $ AmbiguousPane ident n

modifyPane :: (PaneView -> PaneView) -> Ident -> ViewTree -> Either TreeModError ViewTree
modifyPane modification ident tree =
  runIdentity $ runExceptT $ modifyPaneUniqueM (Identity . modification) ident tree

openPane :: Ident -> ViewTree -> Either TreeModError ViewTree
openPane =
  modifyPane paneSetOpen

hasOpenPanes :: ViewTree -> Bool
hasOpenPanes tree =
  has (cosmos . _litTree . leafDataTraversal . filtered isOpen) (LeafIndexTree tree)
  where
    isOpen (View _ _ _ (Pane open _ _)) = open

depthTraverseTree ::
  ∀ a.
  Show a =>
  Monoid a =>
  (a -> ViewTree -> (a, ViewTree)) ->
  (PaneView -> (a, PaneView)) ->
  ViewTree ->
  (a, ViewTree)
depthTraverseTree fn fl =
  rec
  where
    rec :: ViewTree -> (a, ViewTree)
    rec (Tree l sub) =
      uncurry fn . bimap fold (Tree l) . unzip $ (recSub <$> sub)
    recSub :: ViewTreeSub -> (a, ViewTreeSub)
    recSub (TreeNode t) =
      second TreeNode $ rec t
    recSub (TreeLeaf l) =
      second TreeLeaf $ fl l

data ToggleResult =
  Minimized
  |
  Opened
  |
  NotFound
  |
  Multiple Int
  deriving (Eq, Show)

instance Semigroup ToggleResult where
  NotFound <> a = a
  a <> NotFound = a
  Multiple a <> Multiple b = Multiple (a + b)
  Multiple a <> _ = Multiple (a + 1)
  _ <> Multiple a = Multiple (a + 1)
  _ <> _ = Multiple 2

instance Monoid ToggleResult where
  mempty = NotFound

openPinnedSubs :: ToggleResult -> ViewTree -> (ToggleResult, ViewTree)
openPinnedSubs NotFound t =
  (NotFound, t)
openPinnedSubs Opened (Tree l sub) =
  (Opened, Tree l (openPinnedPane <$> sub))
  where
    openPinnedPane :: ViewTreeSub -> ViewTreeSub
    openPinnedPane (TreeLeaf (View i s g (Pane False True cwd))) =
      TreeLeaf $ View i s g (Pane True True cwd)
    openPinnedPane v =
      v
openPinnedSubs a t =
  (a, t)

checkToggleResult ::
  (Ident -> TreeModError) ->
  (Ident -> Int -> TreeModError) ->
  Ident ->
  ToggleResult ->
  a ->
  Either TreeModError a
checkToggleResult missing ambiguous ident =
  checkResult
  where
    checkResult NotFound result = Left (missing ident)
    checkResult (Multiple n) result = Left (ambiguous ident n)
    checkResult _ result = Right result

togglePaneView :: Ident -> PaneView -> (ToggleResult, PaneView)
togglePaneView ident (View i s g (Pane False p c)) | ident == i =
  (Opened, View i s g (Pane True p c))
togglePaneView ident (View i (ViewState minimized) g (Pane True p c)) | ident == i =
  (Minimized, View i (ViewState (not minimized)) g (Pane False p c))
togglePaneView _ v =
  (NotFound, v)

togglePaneNode :: Ident -> ViewTreeSub -> (ToggleResult, ViewTreeSub)
togglePaneNode ident (TreeLeaf v) =
  second TreeLeaf (togglePaneView ident v)
togglePaneNode _ t =
  (NotFound, t)

togglePane :: Ident -> ViewTree -> Either TreeModError ViewTree
togglePane ident =
  uncurry check . depthTraverseTree openPinnedSubs (togglePaneView ident)
  where
    check = checkToggleResult PaneMissing AmbiguousPane ident

isOpenPaneNode :: ViewTreeSub -> Bool
isOpenPaneNode =
  anyOf (TreeSub.leafData . View.extra . Pane.open) id

openPaneView :: PaneView -> (ToggleResult, PaneView)
openPaneView (View i s g (Pane False p c)) =
  (Opened, View i s g (Pane True p c))
openPaneView v =
  (NotFound, v)

openFirstPaneNode :: ToggleResult -> ViewTreeSub -> (ToggleResult, ViewTreeSub)
openFirstPaneNode NotFound (TreeLeaf v) =
  second TreeLeaf (openPaneView v)
openFirstPaneNode a t =
  (a, t)

-- TODO recurse when opening pane
toggleLayoutNode :: Ident -> ToggleResult -> ViewTree -> (ToggleResult, ViewTree)
toggleLayoutNode ident previous (Tree v@(View i (ViewState minimized) g l) sub) | ident == i =
  first (previous <>) (if open then toggleMinimized else openPane)
  where
    open = any isOpenPaneNode sub
    toggleMinimized =
      (Minimized, Tree (View i (ViewState (not minimized)) g l) sub)
    openPane =
      second (Tree v) (mapAccumL openFirstPaneNode NotFound sub)
toggleLayoutNode _ a t =
  (a, t)

toggleLayout :: Ident -> ViewTree -> Either TreeModError ViewTree
toggleLayout ident =
  uncurry check . depthTraverseTree (uncurry openPinnedSubs .: toggleLayoutNode ident) (NotFound,)
  where
    check = checkToggleResult LayoutMissing AmbiguousLayout ident
