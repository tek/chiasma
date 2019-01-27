{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Chiasma.Lens.Tree where

import Control.Lens (
  Index,
  IxValue,
  Ixed(ix),
  Plated(..),
  Traversal',
  Fold,
  makeClassy_,
  cosmos,
  transform,
  over,
  toListOf,
  each,
  preview,
  )
import Data.Foldable (foldrM)
import Data.Data (Data)
import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Ui.Data.View
import Chiasma.Ui.Lens.Ident (matchIdentP)

newtype NodeIndexTree l p =
  NodeIndexTree {
    nitTree :: Tree l p
  }
  deriving (Eq, Show)

makeClassy_ ''NodeIndexTree

newtype LeafIndexTree l p =
  LeafIndexTree {
    litTree :: Tree l p
  }
  deriving (Eq, Show)

makeClassy_ ''LeafIndexTree

plateWrap :: (Data l, Data p) => (Tree l p -> t l p) -> (t l p -> Tree l p) -> Traversal' (t l p) (t l p)
plateWrap consWrapper unconsWrapper f wrappedTree =
  consWrapper <$> plate g (unconsWrapper wrappedTree)
  where
    g tree = unconsWrapper <$> f (consWrapper tree)

instance (Data l, Data p) => Plated (NodeIndexTree l p) where
  plate = plateWrap NodeIndexTree nitTree

instance (Data l, Data p) => Plated (LeafIndexTree l p) where
  plate = plateWrap LeafIndexTree litTree

type LayoutIndexTree = NodeIndexTree LayoutView PaneView
type PaneIndexTree = LeafIndexTree LayoutView PaneView

type instance Index (NodeIndexTree l p) = Ident
type instance Index (LeafIndexTree l p) = Ident

type instance IxValue (NodeIndexTree l p) = l
type instance IxValue (LeafIndexTree l p) = p

leafDataTraversal :: Traversal' (Tree l p) p
leafDataTraversal = _treeSubs . each . _leafData

leafByIdentTraversal :: Identifiable p => Ident -> Traversal' (Tree l p) p
leafByIdentTraversal ident = leafDataTraversal . matchIdentP ident

instance (Identifiable p, Data l, Data p) => Ixed (LeafIndexTree l p) where
  ix ident = _litTree . leafByIdentTraversal ident

leavesByIdentRecursive :: (Identifiable p, Data l, Data p) => Ident -> Fold (LeafIndexTree l p) p
leavesByIdentRecursive ident = cosmos . ix ident

leafByIdent :: (Identifiable p, Data l, Data p) => Ident -> Tree l p -> Maybe p
leafByIdent ident = preview (leavesByIdentRecursive ident) . LeafIndexTree

leavesByIdent :: (Identifiable p, Data l, Data p) => Ident -> Tree l p -> [p]
leavesByIdent ident = toListOf (leavesByIdentRecursive ident) . LeafIndexTree

modifyLeafByIdent :: (Identifiable p, Data l, Data p) => Ident -> (p -> p) -> Tree l p -> Tree l p
modifyLeafByIdent ident f tree =
  litTree $ (transform $ over (ix ident) f) (LeafIndexTree tree)

-- subtreesWithLayout :: Traversal' (Tree l p) (l, TreeSub l p)
subtreesWithLayout :: ∀ l p m. Monad m => ((l, TreeSub l p) -> m (l, TreeSub l p)) -> Tree l p -> m (Tree l p)
subtreesWithLayout f (Tree l0 sub) = do
  (newL, newSub) <- foldrM applySub (l0, []) sub
  return (Tree newL newSub)
  where
    prependSub s (newL, newN) = (newL, newN : s)
    applySub :: TreeSub l p -> (l, [TreeSub l p]) -> m (l, [TreeSub l p])
    applySub (TreeNode t) (l, s) = do
      rec <- (\rsub -> (l, TreeNode rsub)) <$> subtreesWithLayout f t
      (fmap (prependSub s) . f) rec
    applySub p (l, s) =
      (fmap (prependSub s) . f) (l, p)

subtrees :: ∀ l p m. Monad m => (TreeSub l p -> m (TreeSub l p)) -> Tree l p -> m (Tree l p)
subtrees f (Tree l sub) = do
  newSub <- mapM applySub sub
  return (Tree l newSub)
  where
    applySub :: TreeSub l p -> m (TreeSub l p)
    applySub (TreeNode t) = do
      rec <- subtrees f t
      f (TreeNode rec)
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
      rec <- treesAndSubs ft fs t
      fs (TreeNode rec)
    applySub p = fs p
