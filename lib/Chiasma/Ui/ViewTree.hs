module Chiasma.Ui.ViewTree(
  toggleLayout,
  togglePane,
  hasOpenPanes,
) where

import Control.Lens (transformM, mapMOf, ix, cosmos, filtered, has)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(..))
import Data.Traversable (mapAccumL)
import Data.Monoid (Sum(..))
import Chiasma.Data.Ident (Ident)
import Chiasma.Lens.Tree (
  leafByIdent,
  modifyLeafByIdent,
  treesAndSubs,
  _litTree,
  LeafIndexTree(..),
  leafDataTraversal,
  )
import Chiasma.Ui.Data.TreeModError (TreeModError(PaneMissing, AmbiguousPane, LayoutMissing, AmbiguousLayout))
import Chiasma.Ui.Data.View (Tree(Tree), TreeSub(TreeLeaf), LayoutView, PaneView, ViewTree, View(..), Pane(..))
import Chiasma.Ui.Pane (paneToggleOpen)

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

togglePane :: Ident -> ViewTree -> Either TreeModError ViewTree
togglePane ident tree =
  runIdentity $ runExceptT $ modifyPaneUniqueM (Identity . paneToggleOpen) ident tree

hasOpenPanes :: ViewTree -> Bool
hasOpenPanes tree =
  has (cosmos . _litTree . leafDataTraversal . filtered isOpen) (LeafIndexTree tree)
  where
    isOpen (View _ _ _ (Pane open _ _)) = open
