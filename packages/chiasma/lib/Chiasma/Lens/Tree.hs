module Chiasma.Lens.Tree where

import Control.Lens (Fold, Index, IxValue, Ixed (ix), Plated (..), cosmos, makeClassy_, preview, transform)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Prelude hiding (ix, transform)

import Chiasma.Data.Ident (Ident, Identifiable (..))
import Chiasma.Ui.Data.View (LayoutView, PaneView, Tree (Tree), TreeSub (TreeNode))
import Chiasma.Ui.Lens.Ident (matchIdentP)

newtype NodeIndexTree l p =
  NodeIndexTree {
    nitTree :: Tree l p
  }
  deriving stock (Eq, Show)

makeClassy_ ''NodeIndexTree

newtype LeafIndexTree l p =
  LeafIndexTree {
    litTree :: Tree l p
  }
  deriving stock (Eq, Show)

makeClassy_ ''LeafIndexTree

plateWrap :: (Data l, Data p) => (Tree l p -> t l p) -> (t l p -> Tree l p) -> Traversal' (t l p) (t l p)
plateWrap consWrapper unconsWrapper f wrappedTree =
  consWrapper <$> plate g (unconsWrapper wrappedTree)
  where
    g tree' = unconsWrapper <$> f (consWrapper tree')

instance (Data l, Data p) => Plated (NodeIndexTree l p) where
  plate = plateWrap NodeIndexTree (.nitTree)

instance (Data l, Data p) => Plated (LeafIndexTree l p) where
  plate = plateWrap LeafIndexTree (.litTree)

type LayoutIndexTree = NodeIndexTree LayoutView PaneView
type PaneIndexTree = LeafIndexTree LayoutView PaneView

type instance Index (NodeIndexTree _ _) = Ident
type instance Index (LeafIndexTree _ _) = Ident

type instance IxValue (NodeIndexTree l _) = l
type instance IxValue (LeafIndexTree _ p) = p

leafDataTraversal :: Traversal' (Tree l p) p
leafDataTraversal = #treeSubs . each . #_TreeLeaf

leafByIdentTraversal :: Identifiable p => Ident -> Traversal' (Tree l p) p
leafByIdentTraversal ident' = leafDataTraversal . matchIdentP ident'

instance Identifiable p => Ixed (LeafIndexTree l p) where
  ix ident' = _litTree . leafByIdentTraversal ident'

leavesByIdentRecursive :: (Identifiable p, Data l, Data p) => Ident -> Fold (LeafIndexTree l p) p
leavesByIdentRecursive ident' = cosmos . ix ident'

leafByIdent :: (Identifiable p, Data l, Data p) => Ident -> Tree l p -> Maybe p
leafByIdent ident' = preview (leavesByIdentRecursive ident') . LeafIndexTree

leavesByIdent :: (Identifiable p, Data l, Data p) => Ident -> Tree l p -> [p]
leavesByIdent ident' = toListOf (leavesByIdentRecursive ident') . LeafIndexTree

modifyLeafByIdent :: (Identifiable p, Data l, Data p) => Ident -> (p -> p) -> Tree l p -> Tree l p
modifyLeafByIdent ident' f tree' =
  (.litTree) $ (transform $ over (ix ident') f) (LeafIndexTree tree')

subtreesWithLayout :: ∀ l p m. Monad m => ((l, TreeSub l p) -> m (l, TreeSub l p)) -> Tree l p -> m (Tree l p)
subtreesWithLayout f (Tree l0 sub) = do
  (newL, newSub) <- foldrM applySub (l0, []) sub
  pure (Tree newL newSub)
  where
    prependSub s (newL, newN) = (newL, newN : s)
    applySub :: TreeSub l p -> (l, [TreeSub l p]) -> m (l, [TreeSub l p])
    applySub (TreeNode t) (l, s) = do
      recur <- (\rsub -> (l, TreeNode rsub)) <$> subtreesWithLayout f t
      (fmap (prependSub s) . f) recur
    applySub p (l, s) =
      (fmap (prependSub s) . f) (l, p)

subtrees :: ∀ l p m. Monad m => (TreeSub l p -> m (TreeSub l p)) -> Tree l p -> m (Tree l p)
subtrees f (Tree l sub) = do
  newSub <- mapM applySub sub
  pure (Tree l newSub)
  where
    applySub :: TreeSub l p -> m (TreeSub l p)
    applySub (TreeNode t) = do
      recur <- subtrees f t
      f (TreeNode recur)
    applySub p = f p

treesAndSubs ::
  Monad m =>
  (Tree l p -> m (Tree l p)) ->
  (TreeSub l p -> m (TreeSub l p)) ->
  Tree l p ->
  m (Tree l p)
treesAndSubs ft fs (Tree l sub) = do
  treeResult <- mapM applySub sub
  ft (Tree l treeResult)
  where
    applySub (TreeNode t) = do
      recur <- treesAndSubs ft fs t
      fs (TreeNode recur)
    applySub p = fs p
