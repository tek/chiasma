module Chiasma.Ui.ShowTree(
  formatViewTree,
  showTree,
  showViewTree,
  printViewTree,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Chiasma.Data.Ident (identString)
import Chiasma.Ui.Data.View (
  Tree(Tree),
  TreeSub(TreeNode,
  TreeLeaf),
  ViewTree,
  PaneView,
  LayoutView,
  View(View),
  Pane(Pane),
  )

formatLayout :: LayoutView -> String
formatLayout (View ident _ _ _) = "l: " ++ identString ident

formatPane :: PaneView -> String
formatPane (View ident _ _ (Pane open _ _)) =
  "p: " ++ identString ident ++ openFrag
  where
    openFrag = " " ++ if open then "open" else "closed"

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

printViewTree :: MonadIO m => ViewTree -> m ()
printViewTree = liftIO . traverse_ putStrLn . showViewTree
