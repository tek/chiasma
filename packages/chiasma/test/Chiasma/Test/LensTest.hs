module Chiasma.Test.LensTest where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Lens.Tree (leafByIdent, modifyLeafByIdent, treesAndSubs)
import Chiasma.Ui.Data.View (
  Pane(Pane),
  PaneView,
  Tree(Tree),
  TreeSub(TreeNode, TreeLeaf),
  View(View),
  ViewTree,
  ViewTreeSub,
  consLayout,
  consPane,
  )
import qualified Chiasma.Ui.Data.View as View (_ident, ident)
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.ViewTree (togglePane)
import qualified Chiasma.Ui.ViewTree as ToggleResult (ToggleResult(..))
import Control.Lens (transformM)
import qualified Control.Lens as Lens (set)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)
import Chiasma.Test.Util (UnitTest, unitTest)

id0, id1, id2, id3, id4 :: Ident
id0 = Str "0"
id1 = Str "1"
id2 = Str "2"
id3 = Str "3"
id4 = Str "4"

tree :: ViewTree
tree =
  Tree (consLayout id0) [subtree, TreeLeaf (consPane id2)]
  where
    subtree = st id2 subtree2
    subtree2 = st id3 subtree3
    subtree3 = st id4 (TreeLeaf openPane)
    openPane = View id1 (ViewState False) def (Pane True False Nothing)
    st i s =
      TreeNode $ Tree (consLayout i) [s]

test_modify :: UnitTest
test_modify = do
  let
    ident = Str "changed"
    modded = modifyLeafByIdent id1 (Lens.set View.ident ident) tree
  Nothing === leafByIdent ident tree
  Just ident === (View._ident <$> leafByIdent ident modded)

failOnPaneIdent :: Ident -> ViewTree -> Maybe ViewTree
failOnPaneIdent target t@(Tree _ sub) =
  t <$ traverse match sub
  where
    match (TreeLeaf (View i _ _ _)) = if target == i then Nothing else Just ()
    match _ = Just ()

test_monadicModify :: UnitTest
test_monadicModify = do
  Nothing === (transformM (failOnPaneIdent id2) tree)
  Just tree === (transformM (failOnPaneIdent id4) tree)

insertPane :: Ident -> PaneView -> ViewTree -> ViewTree
insertPane targetLayout pane (Tree l sub) =
  if View._ident l == targetLayout then Tree l (TreeLeaf pane : sub) else Tree l sub

ensurePaneUnique :: Ident -> ViewTreeSub -> Maybe ViewTreeSub
ensurePaneUnique paneIdent (TreeLeaf (View ident _ _ _)) | ident == paneIdent = Nothing
ensurePaneUnique _ n = Just n

subtreesTarget :: ViewTree
subtreesTarget =
  Tree (consLayout id0) [subtree, TreeLeaf (consPane id2)]
  where
    subtree = TreeNode $ Tree (consLayout id2) [TreeLeaf $ consPane id4, subtree2]
    subtree2 = TreeNode $ Tree (consLayout id3) [subtree3]
    subtree3 = TreeNode $ Tree (consLayout id4) [TreeLeaf openPane]
    openPane = View id1 (ViewState False) def (Pane True False Nothing)

test_subtrees :: UnitTest
test_subtrees =
  Just subtreesTarget === treesAndSubs (Just . insertPane id2 (consPane id4)) (ensurePaneUnique id4) tree

togglePaneTree :: ViewTree
togglePaneTree =
  Tree (consLayout id0) [TreeLeaf (consPane id0), TreeLeaf (consPane id0), TreeLeaf (consPane id2)]

test_togglePane :: UnitTest
test_togglePane = do
  ToggleResult.Ambiguous 2 === togglePane id0 togglePaneTree
  ToggleResult.NotFound === togglePane id1 togglePaneTree
  ToggleResult.Success (1 :: Int) === (1 <$ togglePane id2 togglePaneTree)

test_lenses :: TestTree
test_lenses =
  testGroup "lenses" [
    unitTest "modify leaves by ident" test_modify,
    unitTest "monadically transform leaves" test_monadicModify,
    unitTest "traverse all subtrees" test_subtrees,
    unitTest "toggle a pane" test_togglePane
  ]
