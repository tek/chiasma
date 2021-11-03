module Chiasma.Ui.Data.RenderableTree where

import Prettyprinter (Doc, Pretty (..), emptyDoc, space, (<+>))

import Chiasma.Data.Axis (Axis)
import Chiasma.Data.TmuxId (PaneId (..))
import Chiasma.Ui.Data.Tree (NNode, NTree)
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (ViewGeometry))
import Chiasma.Ui.Data.ViewState (ViewState)

data RLayout =
  RLayout {
    _ref :: RPane,
    _axis :: Axis
  }
  deriving stock (Eq, Show)

data RPane =
  RPane {
    _id :: PaneId,
    _top :: Int,
    _left :: Int
  }
  deriving stock (Eq, Show)

data Renderable a =
  Renderable {
    _state :: ViewState,
    _geometry :: ViewGeometry,
    _view :: a
  }
  deriving stock (Eq, Show)

type RenderableLayout = Renderable RLayout
type RenderablePane = Renderable RPane
type RenderableTree = NTree RenderableLayout RenderablePane
type RenderableNode = NNode RenderableLayout RenderablePane

instance Pretty RLayout where
  pretty (RLayout (RPane (PaneId refId) _ _) axis) =
    "l –" <+> "ref:" <+> pretty refId <+> "pos:" <+> pretty axis

instance Pretty RPane where
  pretty (RPane (PaneId paneId) top left) =
    "p –" <+> pretty paneId <+> pretty top <+> pretty left

mayPretty :: Text -> Maybe Float -> Doc a
mayPretty prefix (Just a) =
  space <> pretty (prefix <> ":") <+> pretty a
mayPretty _ Nothing =
  emptyDoc

instance Pretty a => Pretty (Renderable a) where
  pretty (Renderable _ (ViewGeometry minSize maxSize fixedSize _ _ _) a) =
    pretty a <> mayPretty "min" minSize <> mayPretty "max" maxSize <> mayPretty "fixed" fixedSize
