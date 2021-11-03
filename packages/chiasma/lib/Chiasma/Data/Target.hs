module Chiasma.Data.Target where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs))
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId, formatId)

data Target =
  Pane PaneId
  |
  Window WindowId
  |
  Session SessionId
  |
  Current
  deriving stock (Eq, Show)

formatTarget :: [Text] -> Target -> [Text]
formatTarget pre = \case
    Pane i -> pre <> [formatId i]
    Window i -> pre <> [formatId i]
    Session i -> pre <> [formatId i]
    Current -> []

instance CmdArgs Target where
  cmdArgs = \case
    Pane i -> ["-t", formatId i]
    Window i -> ["-t", formatId i]
    Session i -> ["-t", formatId i]
    Current -> []

instance Default Target where
  def =
    Current
