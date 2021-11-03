module Chiasma.Data.KillPaneParams where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1)
import Chiasma.Data.Target (Target)

data KillPaneParams =
  KillPaneParams {
    allbut :: Bool,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default KillPaneParams where
  def =
    KillPaneParams {
      allbut = False,
      target = def
    }

instance CmdArgs KillPaneParams where
  cmdArgs KillPaneParams {..} =
    flag1 "-a" allbut
    <>
    cmdArgs target
