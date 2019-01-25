module Chiasma.Ui.ShowTree(
  formatViewTree,
  showTree,
  showViewTree,
) where

import Data.Bifunctor (bimap)
import Chiasma.Data.Ident (identString)
import Chiasma.Ui.Data.View (Tree(Tree), TreeSub(TreeNode, TreeLeaf), ViewTree, PaneView, LayoutView, View(View))

formatLayout :: LayoutView -> String
formatLayout (View ident _ _ _) = "l: " ++ identString ident

formatPane :: PaneView -> String
formatPane (View ident _ _ _) = "p: " ++ identString ident

formatViewTree :: ViewTree -> Tree String String
formatViewTree = bimap formatLayout formatPane

indent :: [String] -> [String]
indent = fmap (" " ++)

showTreeSub :: TreeSub String String -> [String]
showTreeSub (TreeNode tree) = showTree tree
showTreeSub (TreeLeaf pane) = [pane]

showTree :: Tree String String -> [String]
showTree (Tree l sub) =
  l : indent (sub >>= showTreeSub)

showViewTree :: ViewTree -> [String]
showViewTree = showTree . formatViewTree
