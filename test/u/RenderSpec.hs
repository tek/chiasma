{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RenderSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Data.Default.Class (Default(def))
import Data.Foldable (traverse_)
import Data.Text.Prettyprint.Doc ((<>), line)
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Test.Framework
import UnliftIO (throwString)
import UnliftIO.Directory (getCurrentDirectory)

import qualified Chiasma.Codec.Data as Codec (Pane(..))
import Chiasma.Command.Pane (panes)
import Chiasma.Command.Session (activateSession, sessions)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxId (SessionId(SessionId), WindowId(WindowId), PaneId(PaneId))
import Chiasma.Data.TmuxThunk (TmuxError)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views), viewsLog)
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

renderOnce :: ViewTree -> TmuxNative -> FilePath -> Views -> IO Views
renderOnce tree api cwd vs = do
  r <- runTmux api $ runStateT (runExceptT $ render cwd id1 id1 tree) vs
  either (throwString . show) (return . snd) r

runRender :: ViewTree -> TmuxNative -> IO (Either TmuxError [Codec.Pane])
runRender tree api = do
  cwd <- getCurrentDirectory
  vs1 <- renderOnce tree api cwd views
  runTmux api $ activateSession 0
  runTmux api $ activateSession 1
  vs2 <- renderOnce tree api cwd vs1
  sleep 0.5
  -- traverse_ (putDocW 100) (fmap (<> line) $ reverse $ viewsLog vs2)
  runTmux api panes

renderSpec :: ViewTree -> [Codec.Pane] -> IO ()
renderSpec tree target = do
  ps <- tmuxSpec $ runRender tree
  assertEqual (Right target) ps

treeImbalanced :: ViewTree
treeImbalanced =
  Tree (consLayout id0) [
    TreeLeaf (openPane id0 def),
    TreeLeaf (openPane id1 (ViewGeometry Nothing Nothing (Just 150) Nothing Nothing Nothing))
    ]
  where
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

imbalancedTarget :: [Codec.Pane]
imbalancedTarget =
  [Codec.Pane (PaneId 0) 200 50, Codec.Pane (PaneId 1) 49 50, Codec.Pane (PaneId 2) 150 50]

test_imbalanced :: IO ()
test_imbalanced =
  renderSpec treeImbalanced imbalancedTarget

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

nestedTarget :: [Codec.Pane]
nestedTarget =
  [
  Codec.Pane (PaneId 0) 200 50,
  Codec.Pane (PaneId 1) 100 50,
  Codec.Pane (PaneId 2) 99 25,
  Codec.Pane (PaneId 3) 99 24
  ]

test_nested :: IO ()
test_nested =
  renderSpec treeNested nestedTarget

treeTwoLayouts :: ViewTree
treeTwoLayouts =
  Tree (consLayout id0) [
    TreeNode $
      Tree (consLayoutVertical id1) [
        TreeLeaf (openPane id0 def)
        ],
    TreeNode $
      Tree (consLayoutVertical id2) [
        TreeLeaf (openPane id1 def)
        ]
    ]
  where
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

twoLayoutsTarget :: [Codec.Pane]
twoLayoutsTarget =
  [Codec.Pane (PaneId 0) 200 50, Codec.Pane (PaneId 1) 100 50, Codec.Pane (PaneId 2) 99 50]

test_twoLayouts :: IO ()
test_twoLayouts =
  renderSpec treeTwoLayouts twoLayoutsTarget
