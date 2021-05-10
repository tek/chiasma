module Chiasma.Test.RenderTest where

import Chiasma.Codec.Data.PaneDetail (PaneDetail(..))
import Chiasma.Command.Pane (panesAs)
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
import Chiasma.Ui.Data.View (Tree(..), TreeSub(..), ViewTree, consLayout, consLayoutVertical)
import qualified Chiasma.Ui.Data.View as Ui (Pane(Pane), View(View))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(..))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.ViewTree (togglePane)
import qualified Chiasma.Ui.ViewTree as ToggleResult (ToggleResult(Success))
import Hedgehog (evalEither, (===))
import Test.Tasty (TestTree, testGroup)
import UnliftIO (throwString)
import UnliftIO.Directory (getCurrentDirectory)

import Chiasma.Test.Tmux (sleep, tmuxTest)
import Chiasma.Test.Util (UnitTest, integrationTest)

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

renderOnce :: ViewTree -> TmuxNative -> FilePath -> Views -> IO Views
renderOnce tree api cwd vs = do
  r <- runExceptT $ runStateT (runTmux api $ render cwd id1 id1 tree) vs
  either (throwString . show) (return . snd) r

safeRender :: Views -> ViewTree -> TmuxNative -> IO Views
safeRender vs tree api = do
  cwd <- getCurrentDirectory
  vs1 <- renderOnce tree api cwd vs
  _ <- runExceptT @TmuxError $ runTmux api $ activateSession 0
  _ <- runExceptT @TmuxError $ runTmux api $ activateSession 1
  sleep 1
  renderOnce tree api cwd vs1

runRender :: ViewTree -> TmuxNative -> IO ([Tmux.View PaneId], Either TmuxError [PaneDetail])
runRender tree api = do
  (Views _ _ vs2 _) <- safeRender views tree api
  sleep 1
  ps <- runExceptT @TmuxError $ runTmux api panesAs
  return (sortOn viewIdent vs2, sortOn paneLeft . drop 1 <$> ps)

renderTest :: ViewTree -> [PaneDetail] -> UnitTest
renderTest tree target = do
  (_, pse) <- liftIO $ tmuxTest $ runRender tree
  ps <- evalEither pse
  target === ps

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
    PaneDetail { paneId = PaneId 1, paneWidth = 89, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 150, paneHeight = 60, paneTop = 0, paneLeft = 90 }
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
    openPane i geo = Ui.View i (ViewState False) geo (Ui.Pane True False Nothing)

nestedTarget :: [PaneDetail]
nestedTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 120, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 119, paneHeight = 30, paneTop = 0, paneLeft = 121 },
    PaneDetail { paneId = PaneId 3, paneWidth = 119, paneHeight = 29, paneTop = 31, paneLeft = 121 }
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

twoLayoutsTarget :: [PaneDetail]
twoLayoutsTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 120, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 119, paneHeight = 60, paneTop = 0, paneLeft = 121 }
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

positionTarget :: [PaneDetail]
positionTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 10, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 30, paneHeight = 60, paneTop = 0, paneLeft = 11 },
    PaneDetail { paneId = PaneId 3, paneWidth = 177, paneHeight = 60, paneTop = 0, paneLeft = 42 },
    PaneDetail { paneId = PaneId 4, paneWidth = 20, paneHeight = 60, paneTop = 0, paneLeft = 220 }
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

successiveOpenTarget :: [PaneDetail]
successiveOpenTarget =
  [
    PaneDetail { paneId = PaneId 1, paneWidth = 120, paneHeight = 60, paneTop = 0, paneLeft = 0 },
    PaneDetail { paneId = PaneId 2, paneWidth = 119, paneHeight = 30, paneTop = 0, paneLeft = 121 },
    PaneDetail { paneId = PaneId 3, paneWidth = 119, paneHeight = 29, paneTop = 31, paneLeft = 121 }
    ]

test_successiveOpen :: UnitTest
test_successiveOpen = do
  ps <- liftIO $ tmuxTest \ api -> do
    vs1 <- safeRender views treeSuccessiveOpen api
    sleep 0.2
    vs2 <- safeRender vs1 tree1 api
    sleep 0.2
    _ <- safeRender vs2 tree2 api
    sleep 0.2
    runExceptT @TmuxError $ runTmux api panesAs
  ps1 <- evalEither (sortOn paneId . drop 1 <$> ps)
  successiveOpenTarget === ps1
  where
    (ToggleResult.Success tree2) = togglePane id2 tree1
    (ToggleResult.Success tree1) = togglePane id1 treeSuccessiveOpen

test_render :: TestTree
test_render =
  testGroup "render" [
    integrationTest "imbalanced tree" test_imbalanced,
    integrationTest "nested panes" test_nested,
    integrationTest "two layouts" test_twoLayouts,
    integrationTest "fixed pane position" test_position,
    integrationTest "open multiple panes" test_successiveOpen
  ]
