module Chiasma.Ui.ShowTree where

import Chiasma.Data.Ident (identText)
import Chiasma.Ui.Data.View (
  LayoutView,
  Pane(Pane),
  PaneView,
  Tree(Tree),
  TreeSub(TreeNode, TreeLeaf),
  View(View),
  ViewTree,
  )

formatLayout :: LayoutView -> Text
formatLayout (View ident _ _ _) = "l: " <> identText ident

formatPane :: PaneView -> Text
formatPane (View ident _ _ (Pane open _ _)) =
  "p: " <> identText ident <> openFrag
  where
    openFrag = " " <> if open then "open" else "closed"

formatViewTree :: ViewTree -> Tree Text Text
formatViewTree = bimap formatLayout formatPane

indent :: [Text] -> [Text]
indent = fmap (" " <>)

showTreeSub :: TreeSub Text Text -> [Text]
showTreeSub (TreeNode tree) = showTree tree
showTreeSub (TreeLeaf pane) = [pane]

showTree :: Tree Text Text -> [Text]
showTree (Tree l sub) =
  l : indent (sub >>= showTreeSub)

showViewTree :: ViewTree -> [Text]
showViewTree = showTree . formatViewTree

printViewTree :: MonadIO m => ViewTree -> m ()
printViewTree = liftIO . traverse_ putStrLn . showViewTree
