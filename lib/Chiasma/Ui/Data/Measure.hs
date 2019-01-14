{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chiasma.Ui.Data.Measure where

import Control.Lens (makeClassy_)
import Chiasma.Ui.Data.View (Tree, LayoutView, PaneView, TreeSub(..))

data Measured a =
  Measured {
    measuredView :: a,
    measuredSize :: Int
  }
  deriving (Eq, Show)

makeClassy_ ''Measured

type MeasureTree = Tree (Measured LayoutView) (Measured PaneView)
type MeasureTreeSub = TreeSub (Measured LayoutView) (Measured PaneView)
