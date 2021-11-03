module Chiasma.Data.SelectParams where

import Prelude hiding (input, last)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1, option, optionArgs)
import Chiasma.Data.Direction (Direction)
import Chiasma.Data.Target (Target)

data SelectInput =
  EnableInput
  |
  DisableInput
  deriving stock (Eq, Show)

instance CmdArgs SelectInput where
  cmdArgs = \case
    EnableInput -> ["-e"]
    DisableInput -> ["-d"]

data SelectMark =
  SelectMark
  |
  SelectUnmark
  deriving stock (Eq, Show)

instance CmdArgs SelectMark where
  cmdArgs = \case
    SelectMark -> ["-m"]
    SelectUnmark -> ["-M"]

data SelectParams =
  SelectParams {
    neighbor :: Maybe Direction,
    last :: Bool,
    input :: Maybe SelectInput,
    mark :: Maybe SelectMark,
    title :: Maybe Text,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default SelectParams where
  def =
    SelectParams {
      neighbor = Nothing,
      last = False,
      input = Nothing,
      mark = Nothing,
      title = Nothing,
      target = def
    }

instance CmdArgs SelectParams where
  cmdArgs SelectParams {..} =
    optionArgs neighbor
    <>
    flag1 "-l" last
    <>
    optionArgs input
    <>
    optionArgs mark
    <>
    option "-T" title
    <>
    cmdArgs target
