{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chiasma.Ui.Data.Measure where

import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Pretty(..), (<+>), Doc, space, emptyDoc)

import Chiasma.Data.Text.Pretty (prettyS)
import Chiasma.Data.TmuxId (PaneId(..))
import Chiasma.Ui.Data.Tree (NTree, NNode)
import Chiasma.Ui.Data.View (LayoutView, PaneView)
import Control.Lens (makeClassy)

data MLayout =
  MLayout {
    _reference :: PaneId,
    _vertical :: Bool
  }
  deriving (Eq, Show)

makeClassy ''MLayout

newtype MPane =
  MPane {
    _paneId :: PaneId
  }
  deriving (Eq, Show)

makeClassy ''MPane

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
  pretty (MLayout (PaneId refId) vertical) =
    pretty (T.pack "l –") <+> pretty (T.pack "ref:") <+> pretty refId <+>
      pretty (T.pack $ if vertical then "v" else "h")

instance Pretty MPane where
  pretty (MPane (PaneId paneId)) =
    pretty (T.pack "p –") <+> pretty paneId

instance Pretty a => Pretty (Measured a) where
  pretty (Measured size a) =
    pretty a <+> prettyS "size:" <+> pretty size
