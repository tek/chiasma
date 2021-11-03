module Chiasma.Data.PipePaneParams where

import Prelude hiding (input, output)

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), arg, flag1)
import Chiasma.Data.Target (Target (Current))

data PipePaneParams =
  PipePaneParams {
    input :: Bool,
    output :: Bool,
    onlyNew :: Bool,
    target :: Target,
    command :: Maybe Text
  }
  deriving stock (Eq, Show)

instance Default PipePaneParams where
  def =
    PipePaneParams {
      input = False,
      output = False,
      onlyNew = False,
      target = Current,
      command = Nothing
    }

instance CmdArgs PipePaneParams where
  cmdArgs PipePaneParams {..} =
    flag1 "-I" input
    <>
    flag1 "-O" output
    <>
    flag1 "-o" onlyNew
    <>
    cmdArgs target
    <>
    arg command
