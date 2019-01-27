module Chiasma.Ui.ViewTree(
  togglePane,
  hasOpenPanes,
) where

import Control.Lens (transformM, mapMOf, ix, cosmos, filtered, has)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Sum(..))
import Chiasma.Data.Ident (Ident)
import Chiasma.Lens.Tree (
  leafByIdent,
  modifyLeafByIdent,
  treesAndSubs,
  _litTree,
  LeafIndexTree(..),
  leafDataTraversal,
  )
import Chiasma.Ui.Data.TreeModError (TreeModError(PaneMissing, AmbiguousPane))
import Chiasma.Ui.Data.View (Tree, LayoutView, PaneView, ViewTree, View(..), Pane(..))
import Chiasma.Ui.Pane (paneToggleOpen)

modLogged :: Monad m => (a -> m a) -> a -> WriterT (Sum Int) m a
modLogged f a = do
  tell (Sum 1)
  lift $ f a

modifyPaneUnique :: Monad m => (PaneView -> m PaneView) -> Ident -> ViewTree -> ExceptT TreeModError m ViewTree
modifyPaneUnique f ident tree = do
  let st = (transformM $ mapMOf (ix ident) (modLogged f)) (LeafIndexTree tree)
  (result, Sum count) <- lift $ runWriterT st
  case count of
    1 -> return $ litTree result
    0 -> throwError $ PaneMissing ident
    n -> throwError $ AmbiguousPane ident n

togglePane :: Ident -> ViewTree -> Either TreeModError ViewTree
togglePane ident tree =
  runIdentity $ runExceptT $ modifyPaneUnique (Identity . paneToggleOpen) ident tree

hasOpenPanes :: ViewTree -> Bool
hasOpenPanes tree =
  has (cosmos . _litTree . leafDataTraversal . filtered isOpen) (LeafIndexTree tree)
  where
    isOpen (View _ _ _ (Pane open _ _)) = open
