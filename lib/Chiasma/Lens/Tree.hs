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
import Data.Data (Data)
import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Ui.Data.View
import Chiasma.Ui.Lens.Ident

type instance Index (Tree l p) = Ident

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
