{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}

module LensSpec(
  htf_thisModulesTests,
) where

import Test.Framework

import Control.Lens (transformM)
import qualified Control.Lens as Lens (set)
import Data.Default.Class (Default(def))
import Data.Either (isRight)
import Data.Foldable (traverse_)

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Lens.Tree (leafByIdent, modifyLeafByIdent, treesAndSubs)
import Chiasma.Ui.Data.TreeModError (TreeModError(PaneMissing, AmbiguousPane))
import Chiasma.Ui.Data.View (
  Pane(Pane),
  PaneView,
  Tree(Tree),
  TreeSub(TreeNode,   TreeLeaf),
  View(View,   viewIdent),
  ViewTree,
  ViewTreeSub,
  _viewIdent,
  consLayout,
  consPane,
  )
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.ShowTree (showViewTree)
import Chiasma.Ui.ViewTree (togglePane)

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
    subtree = TreeNode $ Tree (consLayout id2) [subtree2]
    subtree2 = TreeNode $ Tree (consLayout id3) [subtree3]
    subtree3 = TreeNode $ Tree (consLayout id4) [TreeLeaf openPane]
    openPane = View id1 (ViewState False) def (Pane True False Nothing)

test_modify :: IO ()
test_modify = do
  let
    ident = Str "changed"
    modded = modifyLeafByIdent id1 (Lens.set _viewIdent ident) tree
  assertEqual Nothing $ leafByIdent ident tree
  assertEqual (Just ident) $ viewIdent <$> leafByIdent ident modded

failOnPaneIdent :: Ident -> ViewTree -> Maybe ViewTree
failOnPaneIdent target t@(Tree _ sub) =
  t <$ traverse match sub
  where
    match (TreeLeaf (View i _ _ _)) = if target == i then Nothing else Just ()
    match _ = Just ()

test_monadicModify :: IO ()
test_monadicModify = do
  assertEqual Nothing (transformM (failOnPaneIdent id2) tree)
  assertEqual (Just tree) (transformM (failOnPaneIdent id4) tree)

insertPane :: Ident -> PaneView -> ViewTree -> ViewTree
insertPane targetLayout pane (Tree l sub) =
  if viewIdent l == targetLayout then Tree l (TreeLeaf pane : sub) else Tree l sub

ensurePaneUnique :: Ident -> ViewTreeSub -> Maybe ViewTreeSub
ensurePaneUnique paneIdent (TreeLeaf (View ident _ _ _)) | ident == paneIdent = Nothing
ensurePaneUnique _ n = Just n

test_subtrees :: IO ()
test_subtrees =
  traverse_ putStrLn $ showVt $ treesAndSubs (Just . insertPane id2 (consPane id4)) (ensurePaneUnique id4) tree
  where
    showVt (Just vt) = showViewTree vt
    showVt Nothing = ["no result"]

togglePaneTree :: ViewTree
togglePaneTree =
  Tree (consLayout id0) [TreeLeaf (consPane id0), TreeLeaf (consPane id0), TreeLeaf (consPane id2)]

test_togglePane :: IO ()
test_togglePane = do
  assertEqual (Left $ AmbiguousPane id0 2) (togglePane id0 togglePaneTree)
  assertEqual (Left $ PaneMissing id1) (togglePane id1 togglePaneTree)
  assertBool (isRight $ togglePane id2 togglePaneTree)
