module Chiasma.Data.CopyModeParams where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1)
import Chiasma.Data.Target (Target)

data CopyModeParams =
  CopyModeParams {
    mouseDrag :: Bool,
    scrollUp :: Bool,
    exitBottom :: Bool,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default CopyModeParams where
  def =
    CopyModeParams {
      mouseDrag = False,
      scrollUp = False,
      exitBottom = False,
      target = def
    }

instance CmdArgs CopyModeParams where
  cmdArgs CopyModeParams {..} =
    flag1 "-M" mouseDrag
    <>
    flag1 "-u" scrollUp
    <>
    flag1 "-e" exitBottom
    <>
    cmdArgs target
