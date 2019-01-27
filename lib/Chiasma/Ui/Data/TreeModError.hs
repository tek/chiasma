module Chiasma.Ui.Data.TreeModError(
  TreeModError(..),
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.View (LayoutView, PaneView)

data TreeModError =
  PaneExists PaneView
  |
  LayoutExists LayoutView
  |
  PaneMissing Ident
  |
  LayoutMissing Ident
  |
  AmbiguousPane Ident Int
  |
  AmbiguousLayout Ident Int
  |
  NoTrees
  deriving (Eq, Show)
