module Chiasma.Data.Direction where

import Prelude hiding (Down, Left, Right)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs))

data Direction =
  Up
  |
  Down
  |
  Left
  |
  Right
  deriving stock (Eq, Show)

instance CmdArgs Direction where
  cmdArgs = \case
    Up -> ["-U"]
    Down -> ["-D"]
    Left -> ["-L"]
    Right -> ["-R"]
