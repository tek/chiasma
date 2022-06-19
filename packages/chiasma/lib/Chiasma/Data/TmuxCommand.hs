module Chiasma.Data.TmuxCommand where

import Data.List (dropWhileEnd)
import qualified Data.Text as Text
import Exon (exon)
import Text.Show (Show (showsPrec), showParen, showString, shows)

import Chiasma.Class.CmdArgs (cmdArgs)
import qualified Chiasma.Codec as Codec
import Chiasma.Codec (multi, single)
import Chiasma.Codec.Data.Client (Client)
import Chiasma.Codec.Data.Pane (Pane)
import Chiasma.Codec.Data.Session (Session)
import Chiasma.Codec.Data.Window (Window)
import Chiasma.Data.CapturePaneParams (CapturePaneParams (CapturePaneParams), stripBlank, stripTrailingWs)
import Chiasma.Data.CopyModeParams (CopyModeParams)
import Chiasma.Data.DecodeError (DecodeError)
import Chiasma.Data.KillPaneParams (KillPaneParams)
import Chiasma.Data.PaneSelection (PaneSelection)
import Chiasma.Data.PipePaneParams (PipePaneParams)
import Chiasma.Data.ResizePaneParams (ResizePaneParams)
import Chiasma.Data.SelectParams (SelectParams)
import Chiasma.Data.SelectWindowParams (SelectWindowParams)
import Chiasma.Data.SendKeysParams (SendKeysParams)
import Chiasma.Data.SessionParams (SessionParams)
import Chiasma.Data.SplitParams (JoinPaneParams, SplitWindowParams)
import Chiasma.Data.Target (Target)
import Chiasma.Data.TmuxId (ClientId (ClientId))
import Chiasma.Data.TmuxQuery (TmuxQuery)
import Chiasma.Data.TmuxRequest (TmuxRequest (TmuxRequest))
import Chiasma.Data.WindowParams (WindowParams)
import Chiasma.Data.WindowSelection (WindowSelection)
import Chiasma.Function (applyWhen)

data TmuxCommand :: Type -> Type where
  Fmap :: (a -> b) -> TmuxCommand a -> TmuxCommand b
  ListPanes :: PaneSelection -> TmuxCommand [Pane]
  ListWindows :: WindowSelection -> TmuxCommand [Window]
  ListSessions :: TmuxCommand [Session]
  ListClients :: TmuxCommand [Client]
  SwitchClient :: ClientId -> Target -> TmuxCommand ()
  NewWindow :: WindowParams -> TmuxCommand Window
  SplitWindow :: WindowParams -> SplitWindowParams -> TmuxCommand Pane
  SelectWindow :: SelectWindowParams -> TmuxCommand ()
  NewSession :: SessionParams -> TmuxCommand Session
  CopyMode :: CopyModeParams -> TmuxCommand ()
  SendKeys :: SendKeysParams -> TmuxCommand ()
  SelectPane :: SelectParams -> TmuxCommand ()
  KillPane :: KillPaneParams -> TmuxCommand ()
  MovePane :: JoinPaneParams -> TmuxCommand ()
  ResizePane :: ResizePaneParams -> TmuxCommand ()
  PipePane :: PipePaneParams -> TmuxCommand ()
  CapturePane :: CapturePaneParams -> TmuxCommand [Text]
  KillServer :: TmuxCommand ()

instance Functor TmuxCommand where
  fmap = Fmap

instance Show (TmuxCommand a) where
  showsPrec d = \case
    Fmap _ cmd ->
      showParen (d > 10) [exon|Fmap #{showsPrec 11 cmd}|]
    ListPanes sel ->
      showParen (d > 10) [exon|ListPanes #{showsPrec 11 sel}|]
    ListWindows sel ->
      showParen (d > 10) [exon|ListWindows #{showsPrec 11 sel}|]
    ListClients ->
      showString "ListClients"
    ListSessions ->
      showString "ListSessions"
    SwitchClient c t ->
      showParen (d > 10) [exon|SwitchClient #{showsPrec 11 c} #{showsPrec 11 t}|]
    NewWindow params ->
      showParen (d > 10) [exon|NewWindow #{showsPrec 11 params}|]
    SplitWindow wParams sParams ->
      showParen (d > 10) [exon|SplitWindow #{showsPrec 11 wParams} #{showsPrec 11 sParams}|]
    SelectWindow params ->
      showParen (d > 10) [exon|SelectWindow #{showsPrec 11 params}|]
    NewSession params ->
      showParen (d > 10) [exon|NewSession #{showsPrec 11 params}|]
    CopyMode params ->
      showParen (d > 10) [exon|SelectPane #{showsPrec 11 params}|]
    SendKeys params ->
      showParen (d > 10) [exon|SendKeys #{showsPrec 11 params}|]
    SelectPane paneId ->
      showParen (d > 10) [exon|SelectPane #{shows paneId}|]
    KillPane params ->
      showParen (d > 10) [exon|KillPane #{showsPrec 11 params}|]
    MovePane params ->
      showParen (d > 10) [exon|MovePane #{showsPrec 11 params}|]
    ResizePane params ->
      showParen (d > 10) [exon|ResizePane #{showsPrec 11 params}|]
    PipePane params ->
      showParen (d > 10) [exon|PipePane #{showsPrec 11 params}|]
    CapturePane params ->
      showParen (d > 10) [exon|CapturePane #{showsPrec 11 params}|]
    KillServer ->
      showString "KillServer"

query ::
  ∀ a .
  TmuxCommand a ->
  Maybe TmuxQuery
query = \case
  Fmap _ cmd -> query cmd
  ListPanes _ -> Just (Codec.query @Pane)
  ListWindows _ -> Just (Codec.query @Window)
  ListSessions -> Just (Codec.query @Session)
  ListClients -> Just (Codec.query @Client)
  SwitchClient _ _ -> Nothing
  NewWindow _ -> Just (Codec.query @Window)
  SplitWindow _ _ -> Just (Codec.query @Pane)
  SelectWindow _ -> Nothing
  NewSession _ -> Just (Codec.query @Session)
  CopyMode _ -> Nothing
  SendKeys _ -> Nothing
  SelectPane _ -> Nothing
  KillPane _ -> Nothing
  MovePane _ -> Nothing
  ResizePane _ -> Nothing
  PipePane _ -> Nothing
  CapturePane _ -> Nothing
  KillServer -> Nothing

request :: TmuxCommand a -> Maybe TmuxQuery -> TmuxRequest
request = \case
  Fmap _ c ->
    request c
  ListPanes selection ->
    TmuxRequest "list-panes" (cmdArgs selection)
  ListWindows selection ->
    TmuxRequest "list-windows" (cmdArgs selection)
  ListSessions ->
    TmuxRequest "list-sessions" []
  ListClients ->
    TmuxRequest "list-clients" []
  SwitchClient (ClientId client) target ->
    TmuxRequest "switch-client" (["-c", client] <> cmdArgs target)
  NewWindow params ->
    TmuxRequest "new-window" (cmdArgs params)
  SplitWindow wParams sParams ->
    TmuxRequest "split-window" (cmdArgs wParams <> cmdArgs sParams)
  SelectWindow params ->
    TmuxRequest "select-window" (cmdArgs params)
  NewSession params ->
    TmuxRequest "new-session" (cmdArgs params)
  CopyMode params ->
    TmuxRequest "copy-mode" (cmdArgs params)
  SendKeys params ->
    TmuxRequest "send-keys" (cmdArgs params)
  SelectPane params ->
    TmuxRequest "select-pane" (cmdArgs params)
  KillPane params ->
    TmuxRequest "kill-pane" (cmdArgs params)
  MovePane params ->
    TmuxRequest "move-pane" (cmdArgs params)
  ResizePane params ->
    TmuxRequest "resize-pane" (cmdArgs params)
  PipePane params ->
    TmuxRequest "pipe-pane" (cmdArgs params)
  CapturePane params ->
    TmuxRequest "capture-pane" (cmdArgs params)
  KillServer ->
    TmuxRequest "kill-server" []

encode :: TmuxCommand a -> TmuxRequest
encode cmd =
  request cmd (query cmd)

decode :: [Text] -> TmuxCommand a -> Either DecodeError a
decode out = \case
  Fmap f cmd ->
    f <$> decode out cmd
  ListPanes _ ->
    multi out
  ListWindows _ ->
    multi out
  ListSessions ->
    multi out
  ListClients ->
    multi out
  SwitchClient _ _ ->
    unit
  NewWindow _ ->
    single out
  SplitWindow _ _ ->
    single out
  SelectWindow _ ->
    unit
  NewSession _ ->
    single out
  CopyMode _ ->
    unit
  SendKeys _ ->
    unit
  SelectPane _ ->
    unit
  KillPane _ ->
    unit
  MovePane _ ->
    unit
  ResizePane _ ->
    unit
  PipePane _ ->
    unit
  CapturePane CapturePaneParams {stripBlank, stripTrailingWs} ->
    pure $
    applyWhen stripBlank (dropWhileEnd ("" ==)) $
    applyWhen stripTrailingWs (fmap Text.stripEnd) $
    out
  KillServer ->
    unit
