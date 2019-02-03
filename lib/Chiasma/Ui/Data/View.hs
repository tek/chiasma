{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Chiasma.Ui.Data.View(
  Pane(..),
  Layout(..),
  View(..),
  PaneView,
  LayoutView,
  Tree(..),
  TreeSub(..),
  ViewTree,
  ViewTreeSub,
  consPane,
  consLayout,
  _extra,
  _viewIdent,
  _viewState,
  _viewGeometry,
  _open,
  _pin,
  _subTree,
  _treeData,
  _treeSubs,
  _leafData,
  consLayoutVertical,
) where

import Control.Lens (
  makeClassy_,
  Index,
  IxValue,
  Ixed(ix),
  )
import Control.Lens.Plated (Plated)
import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Bifunctor (Bifunctor(first, second))
import Data.Data (Data)
import Data.Default.Class (Default(def))
import GHC.Generics (Generic)

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.Lens.Ident (matchIdentP)

data Pane =
  Pane {
    open :: Bool,
    pin :: Bool,
    cwd :: Maybe FilePath
  }
  deriving (Eq, Show, Data, Generic)

makeClassy_ ''Pane

newtype Layout =
  Layout {
    vertical :: Bool
  }
  deriving (Eq, Show, Data, Generic)

makeClassy_ ''Layout

data View a =
  View {
    viewIdent :: Ident,
    viewState :: ViewState,
    viewGeometry :: ViewGeometry,
    extra :: a
  }
  deriving (Eq, Show, Data, Generic)

makeClassy_ ''View

type PaneView = View Pane
type LayoutView = View Layout

consPane :: Ident -> PaneView
consPane ident' = View ident' (ViewState False) def (Pane False False Nothing)

consLayoutAs :: Bool -> Ident -> LayoutView
consLayoutAs vert ident' = View ident' (ViewState False) def (Layout vert)

consLayout :: Ident -> LayoutView
consLayout =
  consLayoutAs False

consLayoutVertical :: Ident -> LayoutView
consLayoutVertical =
  consLayoutAs True

instance Identifiable (View a) where
  identify = viewIdent

-- split in two so there can be no lone leaves (panes without layout) as type 'Tree'
data Tree l p =
  Tree {
    treeData :: l,
    treeSubs :: [TreeSub l p]
    }
  deriving (Eq, Show, Data, Generic)

instance Bifunctor Tree where
  first f (Tree l sub) = Tree (f l) (fmap (first f) sub)

  second f (Tree l sub) =
    Tree l (fmap (second f) sub)

instance Bifoldable Tree where
  bifoldMap fl fr (Tree l sub) = mappend (fl l) (foldMap (bifoldMap fl fr) sub)

data TreeSub l p =
  TreeNode { subTree :: Tree l p }
  |
  TreeLeaf { leafData :: p }
  deriving (Eq, Show, Data, Generic)

instance Bifunctor TreeSub where
  first f (TreeNode t) = TreeNode (first f t)
  first _ (TreeLeaf p) = TreeLeaf p

  second f (TreeNode t) = TreeNode (second f t)
  second f (TreeLeaf p) = TreeLeaf (f p)

instance Bifoldable TreeSub where
  bifoldMap fl fr (TreeNode t) = bifoldMap fl fr t
  bifoldMap _ fr (TreeLeaf p) = fr p

instance (Data l, Data p) => Plated (Tree l p)

makeClassy_ ''Tree
makeClassy_ ''TreeSub

type ViewTree = Tree LayoutView PaneView
type ViewTreeSub = TreeSub LayoutView PaneView

instance Identifiable l => Identifiable (Tree l p) where
  identify (Tree l _) = identify l

type instance Index (Tree l p) = Ident
type instance IxValue (Tree l p) = Tree l p

instance (Identifiable l, Data l, Data p) => Ixed (Tree l p) where
  ix ident = matchIdentP ident
