module Chiasma.Lens.Tree where

import Control.Lens (Fold, Index, IxValue, Ixed (ix), Plated (..), cosmos, makeClassy_, preview, transform, lens)
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

nodeTraversal :: Traversal' (Tree l p) l
nodeTraversal = #treeSubs . each . #_TreeNode . #treeData

nodeByIdentTraversal :: Identifiable l => Ident -> Traversal' (Tree l p) l
nodeByIdentTraversal ident = nodeTraversal . matchIdentP ident

leafDataTraversal :: Traversal' (Tree l p) p
leafDataTraversal = #treeSubs . each . #_TreeLeaf

leafByIdentTraversal :: Identifiable p => Ident -> Traversal' (Tree l p) p
leafByIdentTraversal ident = leafDataTraversal . matchIdentP ident

instance Identifiable l => Ixed (NodeIndexTree l p) where
  ix ident = _nitTree . nodeByIdentTraversal ident

nodesByIdentRecursive :: (Identifiable l, Data l, Data p) => Ident -> Fold (NodeIndexTree l p) l
nodesByIdentRecursive ident = cosmos . ix ident

nodesIdent ::
  ∀ l p .
  Identifiable l =>
  Data l =>
  Data p =>
  Ident ->
  Fold (Tree l p) l
nodesIdent ident = lens coerce (const (.nitTree)) . nodesByIdentRecursive ident

nodeByIdent ::
  ∀ l p .
  Identifiable l =>
  Data l =>
  Data p =>
  Ident ->
  Tree l p ->
  Maybe l
nodeByIdent ident = preview (nodesIdent ident)

nodesByIdent ::
  ∀ l p .
  Identifiable l =>
  Data l =>
  Data p =>
  Ident ->
  Tree l p ->
  [l]
nodesByIdent ident = toListOf (nodesIdent ident)

instance Identifiable p => Ixed (LeafIndexTree l p) where
  ix ident = _litTree . leafByIdentTraversal ident

leavesByIdentRecursive ::
  ∀ l p .
  Identifiable p =>
  Data l =>
  Data p =>
  Ident ->
  Fold (LeafIndexTree l p) p
leavesByIdentRecursive ident = cosmos . ix ident

leavesIdent ::
  ∀ l p .
  Identifiable p =>
  Data l =>
  Data p =>
  Ident ->
  Fold (Tree l p) p
leavesIdent ident = lens coerce (const (.litTree)) . leavesByIdentRecursive ident

leafByIdent ::
  ∀ l p .
  Identifiable p =>
  Data l =>
  Data p =>
  Ident ->
  Tree l p ->
  Maybe p
leafByIdent ident = preview (leavesIdent ident)

leavesByIdent ::
  ∀ l p .
  Identifiable p =>
  Data l =>
  Data p =>
  Ident ->
  Tree l p ->
  [p]
leavesByIdent ident = toListOf (leavesIdent ident)

modifyLeafByIdent :: (Identifiable p, Data l, Data p) => Ident -> (p -> p) -> Tree l p -> Tree l p
modifyLeafByIdent ident f tree' =
  (.litTree) $ (transform $ over (ix ident) f) (LeafIndexTree tree')

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
