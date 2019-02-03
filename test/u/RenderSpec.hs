{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RenderSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Default.Class (Default(def))
import Test.Framework
import UnliftIO.Directory (getCurrentDirectory)

import qualified Chiasma.Codec.Data as Codec (Pane(..))
import Chiasma.Command.Pane (panes)
import Chiasma.Command.Session (activateSession, sessions)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxId (SessionId(SessionId), WindowId(WindowId), PaneId(PaneId))
import Chiasma.Data.TmuxThunk (TmuxError)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Monad.Stream (TmuxProg, runTmux)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Render (render)
import Chiasma.Test.Tmux (tmuxGuiSpec, tmuxSpec, sleep)
import Chiasma.Ui.Data.View (ViewTree, Tree(..), TreeSub(..), consLayout, consLayoutVertical)
import qualified Chiasma.Ui.Data.View as Ui (View(View, extra), Pane(Pane))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(..))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))

id0, id1, id2 :: Ident
id0 = Str "0"
id1 = Str "1"
id2 = Str "2"

views :: Views
views =
  Views
    [Tmux.View id0 (Just (SessionId 0))]
    [Tmux.View id0 (Just (WindowId 0))]
    [Tmux.View id0 (Just (PaneId 0))]
    []

treeImbalanced :: ViewTree
treeImbalanced =
  Tree (consLayout id0) [
    TreeLeaf (openPane id0 def),
    TreeLeaf (openPane id1 (ViewGeometry Nothing Nothing (Just 150) Nothing Nothing Nothing))
    ]
  where
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

treeNested :: ViewTree
treeNested =
  Tree (consLayoutVertical id0) [
    TreeNode $
      Tree (consLayout id1) [
        TreeLeaf (openPane id0 def),
        TreeNode $
          Tree (consLayoutVertical id2) [
            TreeLeaf (openPane id1 def),
            TreeLeaf (openPane id2 def)
            ]
        ]
    ]
  where
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

runRender :: ViewTree -> TmuxNative -> IO (Either TmuxError [Codec.Pane])
runRender tree api = do
  cwd <- getCurrentDirectory
  let
    st :: StateT Views (TmuxProg IO) (Either RenderError ())
    st = runExceptT $ render cwd id1 id1 tree
    prog :: TmuxProg IO (Either RenderError ())
    prog = evalStateT st views
  runTmux api prog
  runTmux api $ activateSession 1
  sleep 1
  runTmux api panes

test_imbalanced :: IO ()
test_imbalanced = do
  ps <- tmuxSpec $ runRender treeImbalanced
  assertEqual (Right [Codec.Pane (PaneId 0) 200 50, Codec.Pane (PaneId 1) 62 50, Codec.Pane (PaneId 2) 137 50]) ps

nestedTarget :: [Codec.Pane]
nestedTarget =
  [
  Codec.Pane (PaneId 0) 200 50,
  Codec.Pane (PaneId 1) 99 50,
  Codec.Pane (PaneId 2) 100 24,
  Codec.Pane (PaneId 2) 100 25
  ]

test_nested :: IO ()
test_nested = do
  ps <- tmuxGuiSpec $ runRender treeNested
  assertEqual (Right nestedTarget) ps
