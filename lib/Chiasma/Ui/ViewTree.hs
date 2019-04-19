module Chiasma.Ui.ViewTree where

import Control.Lens (cosmos, filtered, has, ix, mapMOf, transformM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Bifunctor (bimap, second)
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

toggleLayout :: Ident -> ViewTree -> Either TreeModError ViewTree
toggleLayout ident tree =
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

openPinnedSubs :: Sum Int -> ViewTree -> (Sum Int, ViewTree)
openPinnedSubs (Sum 0) t =
  (Sum 0, t)
openPinnedSubs (Sum n) (Tree l sub) =
  (Sum n, Tree l (openPinnedPane <$> sub))
  where
    openPinnedPane :: ViewTreeSub -> ViewTreeSub
    openPinnedPane (TreeLeaf (View i s g (Pane False True cwd))) =
      TreeLeaf $ View i s g (Pane True True cwd)
    openPinnedPane v =
      v

togglePane :: Ident -> ViewTree -> Either TreeModError ViewTree
togglePane ident =
  uncurry checkResult . depthTraverseTree openPinnedSubs toggle
  where
    checkResult (Sum 1) result = Right result
    checkResult (Sum 0) result = Left (PaneMissing ident)
    checkResult (Sum n) result = Left (AmbiguousPane ident n)
    toggle (View i s g (Pane False p c)) | ident == i =
      (Sum 1, View i s g (Pane True p c))
    toggle (View i (ViewState minimized) g (Pane True p c)) | ident == i =
      (Sum 0, View i (ViewState (not minimized)) g (Pane False p c))
    toggle v =
      (Sum 0, v)
