module Chiasma.Ui.ViewTree(
  togglePane,
) where

import Control.Lens (transformM, mapMOf, ix)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Sum(..))
import Chiasma.Data.Ident (Ident)
import Chiasma.Lens.Tree (leafByIdent, modifyLeafByIdent, treesAndSubs, litTree, LeafIndexTree(..))
import Chiasma.Ui.Data.TreeModError (TreeModError(PaneMissing, AmbiguousPane))
import Chiasma.Ui.Data.View (Tree, LayoutView, PaneView, ViewTree)
import Chiasma.Ui.Pane (paneToggleOpen)

modLogged :: Monad m => (PaneView -> m PaneView) -> PaneView -> WriterT (Sum Int) m PaneView
modLogged f pane = do
  tell (Sum 1)
  lift $ f pane

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
