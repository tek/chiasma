{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RenderSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Default.Class (Default(def))
import Data.Either.Combinators (fromRight)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Test.Framework
import UnliftIO (throwString)
import UnliftIO.Directory (getCurrentDirectory)

import qualified Chiasma.Codec.Data as Codec (Pane(..))
import Chiasma.Codec.Data.PaneDetail (PaneDetail(..))
import Chiasma.Command.Pane (panes, panesAs)
import Chiasma.Command.Session (activateSession)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId), SessionId(SessionId), WindowId(WindowId))
import Chiasma.Data.View (viewIdent)
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(..))
import Chiasma.Render (render)
import Chiasma.Test.Tmux (sleep, tmuxGuiSpec, tmuxSpec)
import Chiasma.Ui.Data.View (Tree(..), TreeSub(..), ViewTree, consLayout, consLayoutVertical)
import qualified Chiasma.Ui.Data.View as Ui (Pane(Pane), View(View))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(..))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))

id0, id1, id2 :: Ident
id0 = Str "0"
id1 = Str "1"
id2 = Str "2"
id3 = Str "3"

views :: Views
views =
  Views
    [Tmux.View id0 (Just (SessionId 0))]
    [Tmux.View id0 (Just (WindowId 0))]
    [Tmux.View id0 (Just (PaneId 0))]
    []

renderOnce :: ViewTree -> TmuxNative -> FilePath -> Views -> IO Views
renderOnce tree api cwd vs = do
  r <- runExceptT $ runStateT (runTmux api $ render cwd id1 id1 tree) vs
  either (throwString . show) (return . snd) r

runRender :: ViewTree -> TmuxNative -> IO ([Tmux.View PaneId], Either TmuxError [PaneDetail])
runRender tree api = do
  cwd <- getCurrentDirectory
  vs1 <- renderOnce tree api cwd views
  _ <- runExceptT @TmuxError $ runTmux api $ activateSession 0
  _ <- runExceptT @TmuxError $ runTmux api $ activateSession 1
  sleep 1
  Views _ _ vs2 _ <- renderOnce tree api cwd vs1
  sleep 1
  ps <- runExceptT @TmuxError $ runTmux api panesAs
  return (sortOn viewIdent vs2, sortOn paneLeft . drop 1 <$> ps)

renderSpec :: ViewTree -> [PaneDetail] -> IO ()
renderSpec tree target = do
  (_, pse) <- tmuxSpec $ runRender tree
  ps <- assertRight pse
  assertEqual target ps

treeImbalanced :: ViewTree
treeImbalanced =
  Tree (consLayout id0) [
    TreeLeaf (openPane id0 def),
    TreeLeaf (openPane id1 (ViewGeometry Nothing Nothing (Just 150) Nothing Nothing Nothing))
    ]
  where
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

imbalancedTarget :: [PaneDetail]
imbalancedTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 449, paneHeight = 200, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 150, paneHeight = 200, paneTop = 0, paneLeft = 450 }
    ]

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

nestedTarget :: [PaneDetail]
nestedTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 300, paneHeight = 200, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 299, paneHeight = 100, paneTop = 0, paneLeft = 301 },
    PaneDetail { paneId = PaneId 3, paneWidth = 299, paneHeight = 99, paneTop = 101, paneLeft = 301 }
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

twoLayoutsTarget :: [PaneDetail]
twoLayoutsTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 300, paneHeight = 200, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 299, paneHeight = 200, paneTop = 0, paneLeft = 301 }
    ]

test_twoLayouts :: IO ()
test_twoLayouts =
  renderSpec treeTwoLayouts twoLayoutsTarget

treePosition :: ViewTree
treePosition =
  Tree (consLayout id0) [
    TreeLeaf (openPane id0 1),
    TreeLeaf (openPane id1 4),
    TreeLeaf (openPane id2 2),
    TreeLeaf (openPane id3 3)
    ]
  where
    openPane i pos = Ui.View i (ViewState False) def { position = Just pos } (Ui.Pane True False Nothing)

positionTarget :: [PaneDetail]
positionTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 150, paneHeight = 200, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 149, paneHeight = 200, paneTop = 0, paneLeft = 151 },
    PaneDetail { paneId = PaneId 3, paneWidth = 149, paneHeight = 200, paneTop = 0, paneLeft = 301 },
    PaneDetail { paneId = PaneId 4, paneWidth = 149, paneHeight = 200, paneTop = 0, paneLeft = 451 }
    ]

test_position :: IO ()
test_position =
  renderSpec treePosition positionTarget
