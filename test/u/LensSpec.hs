{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}

module LensSpec(
  htf_thisModulesTests,
) where

import Test.Framework

import Control.Lens
import Data.Default.Class (Default(def))
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Lens.Tree (leafByIdent, modifyLeafByIdent)
import Chiasma.Ui.Data.View (
  Tree(Tree),
  TreeSub(TreeNode,
  TreeLeaf),
  ViewTree,
  View(View,
  viewIdent),
  _viewIdent,
  consLayout,
  consPane,
  Pane(Pane),
  )
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))

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
    subtree = TreeNode $ Tree (consLayout id3) [subtree2]
    subtree2 = TreeNode $ Tree (consLayout id4) [subtree3]
    subtree3 = TreeNode $ Tree (consLayout id4) [TreeLeaf openPane]
    openPane = View id1 (ViewState False) def (Pane True False Nothing)

test_modify :: IO ()
test_modify = do
  let
    ident = Str "changed"
    modded = modifyLeafByIdent id1 (set _viewIdent ident) tree
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
