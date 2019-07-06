{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PinSpec (htf_thisModulesTests) where

import Test.Framework

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Ui.Data.View (
  Pane(Pane),
  Tree(Tree),
  TreeSub(TreeNode, TreeLeaf),
  View(View),
  ViewTree,
  ViewTreeSub,
  consLayout,
  )
import Chiasma.Ui.ViewTree (toggleLayout, togglePane)
import qualified Chiasma.Ui.ViewTree as ToggleResult (ToggleResult(..))

node :: ViewTreeSub -> View Pane -> ViewTreeSub
node sub p =
  TreeNode $ Tree (consLayout "l") [sub, TreeLeaf p]

paneWith :: Bool -> Text -> ViewTreeSub
paneWith open i = TreeLeaf $ View (Str i) def def (Pane open False Nothing)

pane :: Text -> ViewTreeSub
pane =
  paneWith False

ppaneWithIdent :: Text -> Bool -> View Pane
ppaneWithIdent name open =
  View (Str name) def def (Pane open True Nothing)

ppaneWith :: Bool -> View Pane
ppaneWith =
  ppaneWithIdent "pin"

ppane :: View Pane
ppane =
  ppaneWith False

subtree4 :: ViewTreeSub
subtree4 = node subtree5 ppane

subtree5 :: ViewTreeSub
subtree5 = node subtree6 ppane

subtree6 :: ViewTreeSub
subtree6 = TreeNode $ Tree (consLayout (Str "l")) [pane "p2"]

tree :: ViewTree
tree =
  Tree (consLayout (Str "root")) [subtree4, subtree1, subtree4]
  where
    subtree1 = node subtree2 ppane
    subtree2 = node subtree3 ppane
    subtree3 = TreeNode $ Tree (consLayout (Str "l1")) [pane "p1"]

target :: ViewTree
target =
  Tree (consLayout (Str "root")) [subtree4, subtree1, subtree4]
  where
    subtree1 = node subtree2 (ppaneWith True)
    subtree2 = node subtree3 (ppaneWith True)
    subtree3 = TreeNode $ Tree (consLayout (Str "l1")) [TreeLeaf $ View (Str "p1") def def (Pane True False Nothing)]

test_pinOpenNonpinned :: IO ()
test_pinOpenNonpinned =
  assertEqual (ToggleResult.Success target) (togglePane (Str "p1") tree)

test_layoutPinOpenNonpinned :: IO ()
test_layoutPinOpenNonpinned =
  assertEqual (ToggleResult.Success target) (toggleLayout (Str "l1") tree)

pinnedTree :: ViewTree
pinnedTree =
  Tree (consLayout (Str "root")) [pane "left", subtree]
  where
    subtree = TreeNode $ Tree (consLayout (Str "l1")) [pane "p2", TreeLeaf $ ppaneWithIdent "p1" False]

pinnedTarget :: ViewTree
pinnedTarget =
  Tree (consLayout (Str "root")) [pane "left", subtree]
  where
    subtree = TreeNode $ Tree (consLayout (Str "l1")) [paneWith False "p2", TreeLeaf $ ppaneWithIdent "p1" True]

test_pinOpenPinned :: IO ()
test_pinOpenPinned =
  assertEqual (ToggleResult.Success pinnedTarget) (toggleLayout (Str "l1") pinnedTree)
