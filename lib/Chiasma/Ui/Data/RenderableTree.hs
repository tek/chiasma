module Chiasma.Ui.Data.RenderableTree where

import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), emptyDoc, space, (<+>))

import Chiasma.Data.Text.Pretty (prettyS)
import Chiasma.Data.TmuxId (PaneId(..))
import Chiasma.Ui.Data.Tree (NNode, NTree)
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))
import Chiasma.Ui.Data.ViewState (ViewState)

data RLayout =
  RLayout {
    _ref :: RPane,
    _vertical :: Bool
  }
  deriving (Eq, Show)

data RPane =
  RPane {
    _id :: PaneId,
    _top :: Int,
    _left :: Int
  }
  deriving (Eq, Show)

data Renderable a =
  Renderable {
    _state :: ViewState,
    _geometry :: ViewGeometry,
    _view :: a
  }
  deriving (Eq, Show)

type RenderableLayout = Renderable RLayout
type RenderablePane = Renderable RPane
type RenderableTree = NTree RenderableLayout RenderablePane
type RenderableNode = NNode RenderableLayout RenderablePane

instance Pretty RLayout where
  pretty (RLayout (RPane (PaneId refId) _ _) vertical) =
    "l –" <+> "ref:" <+> pretty refId <+> "pos:" <+> if vertical then "v" else "h"

instance Pretty RPane where
  pretty (RPane (PaneId paneId) top left) =
    "p –" <+> pretty paneId <+> pretty top <+> pretty left

mayPretty :: String -> Maybe Float -> Doc a
mayPretty prefix (Just a) =
  space <> prettyS (prefix ++ ":") <+> pretty a
mayPretty _ Nothing =
  emptyDoc

instance Pretty a => Pretty (Renderable a) where
  pretty (Renderable _ (ViewGeometry minSize maxSize fixedSize _ _ _) a) =
    pretty a <> mayPretty "min" minSize <> mayPretty "max" maxSize <> mayPretty "fixed" fixedSize
