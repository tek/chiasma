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

test_lens :: IO ()
test_lens = do
  let
    ident = Str "changed"
    modded = modifyLeafByIdent id1 (set _viewIdent ident) tree
  assertEqual Nothing $ leafByIdent ident tree
  assertEqual (Just ident) $ viewIdent <$> leafByIdent ident modded
