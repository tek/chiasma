module Chiasma.Data.Axis where

import Data.Data (Data)
import Prettyprinter (Pretty (pretty))

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs))

data Axis =
  Horizontal
  |
  Vertical
  deriving stock (Eq, Show, Data)

instance Pretty Axis where
  pretty = \case
    Horizontal -> "h"
    Vertical -> "v"

instance CmdArgs Axis where
  cmdArgs = \case
    Horizontal -> ["-h"]
    Vertical -> ["-v"]

instance Default Axis where
  def =
    Vertical

vertical :: Axis -> Bool
vertical = \case
  Horizontal -> False
  Vertical -> True
