module Chiasma.Test.RenderTest where

import Path.IO (getCurrentDir)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (UnitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Chiasma.Codec.Data.Pane (Pane (..))
import Chiasma.Command.Pane (windowPanes)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident (Str))
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import qualified Chiasma.Data.TmuxError as TmuxError
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId (PaneId), SessionId (SessionId), WindowId (WindowId))
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.TmuxResponse (TmuxResponse)
import qualified Chiasma.Data.View as Tmux
import Chiasma.Data.Views (Views (Views))
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxApi (TmuxApi)
import Chiasma.Effect.TmuxClient (ScopedTmux)
import Chiasma.Render (render)
import Chiasma.Test.Tmux (tmuxTest)
import Chiasma.Test.Util (integrationTest)
import Chiasma.Tmux (withTmux, withTmuxApis)
import Chiasma.Ui.Data.View (Tree (..), TreeSub (..), ViewTree, consLayout, consLayoutVertical)
import qualified Chiasma.Ui.Data.View as Ui (Pane (Pane), View (View))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (..))
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))
import Chiasma.Ui.ViewTree (togglePane, toggleResultEither)

id0, id1, id2, id3 :: Ident
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

renderOnce ::
  Member (Codec TmuxCommand TmuxRequest TmuxResponse !! CodecError) r =>
  Member (Codec (Panes Pane) TmuxRequest TmuxResponse !! CodecError) r =>
  Members [ScopedTmux TmuxRequest TmuxResponse, AtomicState Views, Stop RenderError, RunStop, Embed IO] r =>
  ViewTree ->
  Sem r ()
renderOnce tree = do
  cwd <- embed getCurrentDir
  withTmuxApis @[TmuxCommand, Panes _] (render cwd id0 id1 tree)

runRender ::
  ∀ r .
  Member (Codec TmuxCommand TmuxRequest TmuxResponse !! CodecError) r =>
  Member (Codec (Panes Pane) TmuxRequest TmuxResponse !! CodecError) r =>
  Member (ScopedTmux TmuxRequest TmuxResponse) r =>
  Members [Stop RenderError, Stop TmuxError, RunStop, ChronosTime, Embed IO] r =>
  ViewTree ->
  Sem r ([Tmux.View PaneId], [Pane])
runRender tree = do
  (Views _ _ viewsResult _) <- execState views (atomicStateToState (renderOnce tree))
  panesResult <- withTmux (resumeHoist TmuxError.codec (windowPanes 1))
  pure (sortOn (.ident) viewsResult, sortOn (.paneLeft) panesResult)

renderTest ::
  ViewTree ->
  [Pane] ->
  UnitTest
renderTest tree target = do
  tmuxTest do
    (_, ps) <- restop @TmuxError @(Scoped_ _) (runRender tree)
    target === ps

treeImbalanced :: ViewTree
treeImbalanced =
  Tree (consLayout id0) [
    TreeLeaf (openPane id0 def),
    TreeLeaf (openPane id1 (ViewGeometry Nothing Nothing (Just 150) Nothing Nothing Nothing))
    ]
  where
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

imbalancedTarget :: [Pane]
imbalancedTarget =
  [
    Pane { paneId = PaneId 1, paneWidth = 89, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    Pane { paneId = PaneId 2, paneWidth = 150, paneHeight = 60, paneTop = 0, paneLeft = 90 }
  ]

test_imbalanced :: UnitTest
test_imbalanced =
  renderTest treeImbalanced imbalancedTarget

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
    openPane i geo =
      Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

nestedTarget :: [Pane]
nestedTarget =
  [
    Pane { paneId = PaneId 1, paneWidth = 120, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    Pane { paneId = PaneId 2, paneWidth = 119, paneHeight = 30, paneTop = 0, paneLeft = 121 },
    Pane { paneId = PaneId 3, paneWidth = 119, paneHeight = 29, paneTop = 31, paneLeft = 121 }
  ]

test_nested :: UnitTest
test_nested =
  renderTest treeNested nestedTarget

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

twoLayoutsTarget :: [Pane]
twoLayoutsTarget =
  [
    Pane { paneId = PaneId 1, paneWidth = 120, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    Pane { paneId = PaneId 2, paneWidth = 119, paneHeight = 60, paneTop = 0, paneLeft = 121 }
    ]

test_twoLayouts :: UnitTest
test_twoLayouts =
  renderTest treeTwoLayouts twoLayoutsTarget

treePosition :: ViewTree
treePosition =
  Tree (consLayout id0) [
    TreeLeaf (openPane id0 1 (Just 10)),
    TreeLeaf (openPane id1 4 (Just 20)),
    TreeLeaf (openPane id2 2 (Just 30)),
    TreeLeaf (openPane id3 3 Nothing)
    ]
  where
    openPane i pos size =
      Ui.View i (ViewState False) def { position = Just pos, fixedSize = size } (Ui.Pane True False Nothing)

positionTarget :: [Pane]
positionTarget =
  [
    Pane { paneId = PaneId 1, paneWidth = 10, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    Pane { paneId = PaneId 2, paneWidth = 30, paneHeight = 60, paneTop = 0, paneLeft = 11 },
    Pane { paneId = PaneId 3, paneWidth = 177, paneHeight = 60, paneTop = 0, paneLeft = 42 },
    Pane { paneId = PaneId 4, paneWidth = 20, paneHeight = 60, paneTop = 0, paneLeft = 220 }
    ]

test_position :: UnitTest
test_position =
  renderTest treePosition positionTarget

treeSuccessiveOpen :: ViewTree
treeSuccessiveOpen =
  Tree (consLayout id0) [
    TreeNode $ Tree (consLayoutVertical id1) [
      TreeLeaf (pane id0 True False)
      ],
    TreeNode $ Tree (consLayoutVertical id2) [
      TreeLeaf (pane id1 False True),
      TreeLeaf (pane id2 False False)
      ]
    ]
  where
    pane i open pin =
      Ui.View i (ViewState False) def (Ui.Pane open pin Nothing)

successiveOpenTarget :: [Pane]
successiveOpenTarget =
  [
    Pane { paneId = PaneId 1, paneWidth = 120, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    Pane { paneId = PaneId 2, paneWidth = 119, paneHeight = 30, paneTop = 0, paneLeft = 121 },
    Pane { paneId = PaneId 3, paneWidth = 119, paneHeight = 29, paneTop = 31, paneLeft = 121 }
    ]

togglePaneE ::
  Member (Error Text) r =>
  Ident ->
  ViewTree ->
  Sem r (ViewTree)
togglePaneE i t =
  fromEither (first show (toggleResultEither (togglePane i t)))

test_successiveOpen :: UnitTest
test_successiveOpen = do
  tmuxTest do
    restop @TmuxError @(ScopedTmux _ _) do
      evalState views $ atomicStateToState do
        renderOnce treeSuccessiveOpen
        tree1 <- togglePaneE id1 treeSuccessiveOpen
        renderOnce tree1
        tree2 <- togglePaneE id2 tree1
        renderOnce tree2
      panesResult <- withTmux @(Panes Pane) do
        resumeHoist @CodecError @(TmuxApi (Panes Pane)) TmuxError.codec (windowPanes 1)
      successiveOpenTarget === panesResult

test_render :: TestTree
test_render =
  testGroup "render" [
    integrationTest "imbalanced tree" test_imbalanced,
    integrationTest "nested panes" test_nested,
    integrationTest "two layouts" test_twoLayouts,
    integrationTest "fixed pane position" test_position,
    integrationTest "open multiple panes" test_successiveOpen
  ]
