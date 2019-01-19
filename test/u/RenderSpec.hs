{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RenderSpec(
  htf_thisModulesTests,
) where

import Test.Framework
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Default.Class (Default(def))
import Data.Traversable (forM)
import UnliftIO.Directory (getCurrentDirectory)
import Chiasma.Command.Pane (panes)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxId (SessionId(SessionId), WindowId(WindowId), PaneId(PaneId))
import Chiasma.Data.TmuxThunk (TmuxError)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Monad.Stream (TmuxProg, runTmux)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Render (render)
import Chiasma.Test.Tmux (tmuxSpec)
import Chiasma.Ui.Data.View (ViewTree, Tree(..), TreeSub(..), consLayout, consPane)
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import qualified Chiasma.Ui.Data.View as Ui (View(View), Pane(Pane))
import qualified Chiasma.Codec.Data as Codec (Pane)

id0, id1, id2 :: Ident
id0 = Str "0"
id1 = Str "1"
id2 = Str "2"

views :: Views
views =
  Views [Tmux.View id0 (Just (SessionId 0))] [Tmux.View id0 (Just (WindowId 0))] [Tmux.View id0 (Just (PaneId 0))]

tree :: ViewTree
tree =
  Tree (consLayout id0) [TreeLeaf openPane, TreeLeaf (consPane id2)]
  where
    openPane = Ui.View id1 (ViewState False) def (Ui.Pane True False Nothing)

runRender :: TmuxNative -> IO (Either TmuxError ([Codec.Pane], [Codec.Pane], Either RenderError ()))
runRender api = do
  cwd <- getCurrentDirectory
  let
    st :: StateT Views (TmuxProg IO) (Either RenderError ())
    st = runExceptT $ render cwd id0 id0 tree
    prog :: TmuxProg IO (Either RenderError ())
    prog = evalStateT st views
  runTmux api $ do
    before <- panes
    r <- prog
    after <- panes
    return (before, after, r)

test_render :: IO ()
test_render = do
  result <- tmuxSpec runRender
  result' <- forM result $ \(before, after, r) -> do
    print before
    print after
    return r
  assertEqual (Right (Right ())) result'
