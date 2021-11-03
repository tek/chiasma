module Chiasma.Ui.Data.View where

import Control.Lens (Index, IxValue, Ixed (ix), makeClassy, makeClassy_)
import Control.Lens.Plated (Plated)
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Data (Data)
import Prettyprinter (Doc, Pretty (..), emptyDoc, nest, space, vsep, (<+>))

import Chiasma.Data.Axis (Axis (Horizontal, Vertical))
import Chiasma.Data.Ident (Ident, Identifiable (..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))
import Chiasma.Ui.Lens.Ident (matchIdentP)

data Pane =
  Pane {
    _open :: Bool,
    _pin :: Bool,
    _cwd :: Maybe Text
  }
  deriving stock (Eq, Show, Data, Generic)

makeClassy ''Pane

instance Default Pane where
  def = Pane False False Nothing

newtype Layout =
  Layout {
    axis :: Axis
  }
  deriving stock (Eq, Show, Data, Generic)
  deriving newtype (Default)

makeClassy_ ''Layout

data View a =
  View {
    _ident :: Ident,
    _state :: ViewState,
    _geometry :: ViewGeometry,
    _extra :: a
  }
  deriving stock (Eq, Show, Data, Generic)

makeClassy ''View

instance Default a => Default (View a) where
  def = View def def def def

type PaneView = View Pane
type LayoutView = View Layout

instance Pretty Layout where
  pretty = \case
    Layout Vertical ->
      "➡"
    Layout Horizontal ->
      "⬇"

instance Pretty Pane where
  pretty (Pane open' pin' _) =
    (if open' then "🔓" else "🔒") <+> (if pin' then "📌" else emptyDoc)

prettyView :: Doc a -> Ident -> ViewState -> ViewGeometry -> Doc a
prettyView sym ident' (ViewState minimized) geo =
  sym <+> pretty ident' <+> "⎸" <+> (if minimized then "▂" <> space else emptyDoc) <> pretty geo

instance Pretty (View Pane) where
  pretty (View ident' st geo a) =
    prettyView "◳" ident' st geo <> pretty a

instance Pretty (View Layout) where
  pretty (View ident' st geo a) =
    prettyView (pretty a) ident' st geo

consPane :: Ident -> PaneView
consPane ident' = View ident' (ViewState False) def (Pane False False Nothing)

consLayoutAs :: Axis -> Ident -> LayoutView
consLayoutAs axis ident' = View ident' (ViewState False) def (Layout axis)

consLayout :: Ident -> LayoutView
consLayout =
  consLayoutAs Horizontal

consLayoutVertical :: Ident -> LayoutView
consLayoutVertical =
  consLayoutAs Vertical

instance Identifiable (View a) where
  identify = _ident

-- split in two so there can be no lone leaves (panes without layout) as type 'Tree'
data Tree l p =
  Tree {
    treeData :: l,
    treeSubs :: [TreeSub l p]
    }
  deriving stock (Eq, Show, Data, Generic)

instance Bifunctor Tree where
  first f (Tree l sub) = Tree (f l) (fmap (first f) sub)

  second f (Tree l sub) =
    Tree l (fmap (second f) sub)

instance Bifoldable Tree where
  bifoldMap fl fr (Tree l sub) = mappend (fl l) (foldMap (bifoldMap fl fr) sub)

data TreeSub l p =
  TreeNode { _subTree :: Tree l p }
  |
  TreeLeaf { _leafData :: p }
  deriving stock (Eq, Show, Data, Generic)

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
makeClassy ''TreeSub

type ViewTree = Tree LayoutView PaneView
type ViewTreeSub = TreeSub LayoutView PaneView

instance Identifiable l => Identifiable (Tree l p) where
  identify (Tree l _) = identify l

type instance Index (Tree _ _) = Ident
type instance IxValue (Tree l p) = Tree l p

instance Identifiable l => Ixed (Tree l p) where
  ix = matchIdentP

instance (Pretty l, Pretty p) => Pretty (TreeSub l p) where
  pretty (TreeNode a) =
    pretty a
  pretty (TreeLeaf a) =
    pretty a

instance (Pretty l, Pretty p) => Pretty (Tree l p) where
  pretty (Tree l sub) =
    nest 2 . vsep $ pretty l : (pretty <$> sub)
