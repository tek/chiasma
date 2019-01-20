{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
) where

import GHC.Generics (Generic)
import Control.Lens (makeClassy_)
import Control.Lens.Plated (Plated)
import Data.Data (Data)
import Data.Default.Class (Default(def))
import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))

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

consLayout :: Ident -> LayoutView
consLayout ident' = View ident' (ViewState False) def (Layout False)

instance Identifiable (View a) where
  identify = viewIdent

-- split in two so there can be no lone leaves (panes without layout) as type 'Tree'
data Tree l p =
  Tree {
    treeData :: l,
    treeSubs :: [TreeSub l p]
    }
  deriving (Eq, Show, Data, Generic)

data TreeSub l p =
  TreeNode { subTree :: Tree l p }
  |
  TreeLeaf { leafData :: p }
  deriving (Eq, Show, Data, Generic)

instance (Data l, Data p) => Plated (Tree l p)

makeClassy_ ''Tree
makeClassy_ ''TreeSub

type ViewTree = Tree LayoutView PaneView
type ViewTreeSub = TreeSub LayoutView PaneView
