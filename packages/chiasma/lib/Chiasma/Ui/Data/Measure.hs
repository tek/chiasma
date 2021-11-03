module Chiasma.Ui.Data.Measure where

import Control.Lens (makeClassy)
import Prettyprinter (Pretty (..), (<+>))

import Chiasma.Data.Axis (Axis)
import Chiasma.Data.TmuxId (PaneId (..))
import Chiasma.Ui.Data.Tree (NNode, NTree)

data MPane =
  MPane {
    _paneId :: PaneId,
    _mainPosition :: Int,
    _offPosition :: Int
  }
  deriving stock (Eq, Show)

makeClassy ''MPane

data MLayout =
  MLayout {
    _reference :: PaneId,
    _lMainPosition :: Int,
    _lOffPosition :: Int,
    _axis :: Axis
  }
  deriving stock (Eq, Show)

makeClassy ''MLayout

data Measured a =
  Measured {
    _size :: Int,
    _view :: a
  }
  deriving stock (Eq, Show)

makeClassy ''Measured

type MeasureTree = NTree (Measured MLayout) (Measured MPane)
type MeasureTreeSub = NNode (Measured MLayout) (Measured MPane)

instance Pretty MLayout where
  pretty (MLayout (PaneId refId) mainPos offPos axis') =
    "l –" <+> "ref:" <+> pretty refId <+> "pos:" <+> pretty mainPos <+> "(" <> pretty offPos <> ")" <+> pretty axis'

instance Pretty MPane where
  pretty (MPane (PaneId paneId') mainPos offPos) =
    "p –" <+> pretty paneId' <+> "pos:" <+> pretty mainPos <+> "(" <> pretty offPos <> ")"

instance Pretty a => Pretty (Measured a) where
  pretty (Measured size' a) =
    pretty a <+> "size:" <+> pretty size'
