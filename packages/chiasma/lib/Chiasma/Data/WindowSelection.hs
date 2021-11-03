module Chiasma.Data.WindowSelection where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs))
import Chiasma.Data.Target (Target (Current))

data WindowSelection =
  All
  |
  InSession Target
  deriving stock (Eq, Show)

instance CmdArgs WindowSelection where
  cmdArgs = \case
    All -> ["-a"]
    InSession target -> cmdArgs target

instance Default WindowSelection where
  def =
    InSession Current
