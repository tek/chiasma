module Chiasma.Data.SelectWindowParams where

import Prelude hiding (last)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1)
import Chiasma.Data.Target (Target)

data SelectWindowParams =
  SelectWindowParams {
    last :: Bool,
    next :: Bool,
    previous :: Bool,
    toggle :: Bool,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default SelectWindowParams where
  def =
    SelectWindowParams {
      last = False,
      next = False,
      previous = False,
      toggle = False,
      target = def
    }

instance CmdArgs SelectWindowParams where
  cmdArgs SelectWindowParams {..} =
    flag1 "-l" last
    <>
    flag1 "-n" next
    <>
    flag1 "-p" previous
    <>
    flag1 "-T" toggle
    <>
    cmdArgs target
