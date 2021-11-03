module Chiasma.Data.PaneSelection where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs))
import Chiasma.Data.Target (Target (Current))

data PaneSelection =
  All
  |
  InSession Target
  |
  InWindow Target
  deriving stock (Eq, Show)

instance CmdArgs PaneSelection where
  cmdArgs = \case
    All -> ["-a"]
    InSession target -> ["-s"] <> cmdArgs target
    InWindow target -> cmdArgs target

instance Default PaneSelection where
  def =
    InWindow Current
