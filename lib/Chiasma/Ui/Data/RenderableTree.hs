module Chiasma.Ui.Data.RenderableTree where

import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Pretty(..), (<+>), Doc, space, emptyDoc)

import Chiasma.Data.Text.Pretty (prettyS)
import Chiasma.Data.TmuxId (PaneId(..))
import Chiasma.Ui.Data.Tree (NTree, NNode)
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))
import Chiasma.Ui.Data.ViewState (ViewState)

data RLayout =
  RLayout {
    _refId :: PaneId,
    _vertical :: Bool
  }
  deriving (Eq, Show)

newtype RPane =
  RPane {
    _id :: PaneId
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
  pretty (RLayout (PaneId refId) vertical) =
    pretty (T.pack "l –") <+> pretty (T.pack "ref:") <+> pretty refId <+>
      pretty (T.pack $ if vertical then "v" else "h")

instance Pretty RPane where
  pretty (RPane (PaneId paneId)) =
    pretty (T.pack "p –") <+> pretty paneId

mayPretty :: String -> Maybe Float -> Doc a
mayPretty prefix (Just a) =
  space <> prettyS (prefix ++ ":") <+> pretty a
mayPretty prefix Nothing =
  emptyDoc

instance Pretty a => Pretty (Renderable a) where
  pretty (Renderable _ (ViewGeometry minSize maxSize fixedSize _ _ _) a) =
    pretty a <> mayPretty "min" minSize <> mayPretty "max" maxSize <> mayPretty "fixed" fixedSize
