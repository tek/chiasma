{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PinSpec(
  htf_thisModulesTests,
) where

import Control.Lens (transformM)
import qualified Control.Lens as Lens (set)
import Data.Default.Class (Default(def))
import Data.Either (isRight)
import Data.Either.Combinators (fromRight)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Text.Prettyprint.Doc (Doc, pretty)
import Test.Framework

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Lens.Tree (leafByIdent, modifyLeafByIdent, treesAndSubs)
import Chiasma.Ui.Data.TreeModError (TreeModError(PaneMissing, AmbiguousPane))
import Chiasma.Ui.Data.View (
  Pane(Pane),
  PaneView,
  Tree(Tree),
  TreeSub(TreeNode, TreeLeaf),
  View(View, viewIdent),
  ViewTree,
  ViewTreeSub,
  _viewIdent,
  consLayout,
  consPane,
  )
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.ShowTree (showViewTree)
import Chiasma.Ui.ViewTree (togglePane)

node :: ViewTreeSub -> View Pane -> ViewTreeSub
node sub p =
  TreeNode $ Tree (consLayout (Str "l")) [sub, TreeLeaf p]

pane :: String -> ViewTreeSub
pane i = TreeLeaf $ View (Str i) def def def

ppaneWith :: Bool -> View Pane
ppaneWith pin = View (Str "pin") def def (Pane pin True Nothing)

ppane :: View Pane
ppane =
  ppaneWith False

subtree4 :: ViewTreeSub
subtree4 = node subtree5 ppane

subtree5 :: ViewTreeSub
subtree5 = node subtree6 ppane

subtree6 :: ViewTreeSub
subtree6 = TreeNode $ Tree (consLayout (Str "l6")) [pane "p2"]

tree :: ViewTree
tree =
  Tree (consLayout (Str "root")) [subtree4, subtree1, subtree4]
  where
    subtree1 = node subtree2 ppane
    subtree2 = node subtree3 ppane
    subtree3 = TreeNode $ Tree (consLayout (Str "l3")) [pane "p1"]

target :: ViewTree
target =
  Tree (consLayout (Str "root")) [subtree4, subtree1, subtree4]
  where
    subtree1 = node subtree2 (ppaneWith True)
    subtree2 = node subtree3 (ppaneWith True)
    subtree3 = TreeNode $ Tree (consLayout (Str "l3")) [TreeLeaf $ View (Str "p1") def def (Pane True False Nothing)]

test_pinOpen :: IO ()
test_pinOpen =
  assertEqual target =<< assertRight (togglePane (Str "p1") tree)
