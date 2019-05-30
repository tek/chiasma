{-# LANGUAGE TemplateHaskell #-}

module Chiasma.Ui.Data.Measure where

import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Pretty(..), (<+>))

import Chiasma.Data.Text.Pretty (prettyS)
import Chiasma.Data.TmuxId (PaneId(..))
import Chiasma.Ui.Data.Tree (NNode, NTree)
import Control.Lens (makeClassy)

data MPane =
  MPane {
    _paneId :: PaneId,
    _position :: Int
  }
  deriving (Eq, Show)

makeClassy ''MPane

data MLayout =
  MLayout {
    _reference :: PaneId,
    _lPosition :: Int,
    _vertical :: Bool
  }
  deriving (Eq, Show)

makeClassy ''MLayout

data Measured a =
  Measured {
    _size :: Int,
    _view :: a
  }
  deriving (Eq, Show)

makeClassy ''Measured

type MeasureTree = NTree (Measured MLayout) (Measured MPane)
type MeasureTreeSub = NNode (Measured MLayout) (Measured MPane)

instance Pretty MLayout where
  pretty (MLayout (PaneId refId) pos vertical') =
    "l –" <+> "ref:" <+> pretty refId <+> "pos:" <+> pretty pos <+> if vertical' then "v" else "h"

instance Pretty MPane where
  pretty (MPane (PaneId paneId') pos) =
    "p –" <+> pretty paneId' <+> "pos:" <+> pretty pos

instance Pretty a => Pretty (Measured a) where
  pretty (Measured size' a) =
    pretty a <+> "size:" <+> pretty size'
