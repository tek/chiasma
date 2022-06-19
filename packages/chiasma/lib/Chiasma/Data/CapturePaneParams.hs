module Chiasma.Data.CapturePaneParams where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1, optionArgs, optionArgsWith)
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.Target (Target)
import Prelude hiding (output)

data CaptureOutput =
  Stdout
  |
  Buffer Text
  deriving stock (Eq, Show)

instance CmdArgs CaptureOutput where
  cmdArgs = \case
    Stdout -> ["-p"]
    Buffer name -> ["-b", name]

data CaptureLine =
  Edge
  |
  CaptureLine Int
  deriving stock (Eq, Show)

instance CmdArgs CaptureLine where
  cmdArgs = \case
    Edge -> ["-"]
    CaptureLine n -> [show n]

data CapturePaneParams =
  CapturePaneParams {
    output :: Maybe CaptureOutput,
    alternate :: Bool,
    quiet :: Bool,
    escapeSequences :: Bool,
    octal :: Bool,
    joinWrapped :: Bool,
    incomplete :: Bool,
    startLine :: Maybe CaptureLine,
    endLine :: Maybe CaptureLine,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default CapturePaneParams where
  def =
    CapturePaneParams {
      output = Nothing,
      alternate = False,
      quiet = False,
      escapeSequences = False,
      octal = False,
      joinWrapped = True,
      incomplete = False,
      startLine = Nothing,
      endLine = Nothing,
      target = Target.Current
    }

instance CmdArgs CapturePaneParams where
  cmdArgs CapturePaneParams {..} =
    flag1 "-a" alternate
    <>
    flag1 "-q" quiet
    <>
    flag1 "-e" escapeSequences
    <>
    flag1 "-C" octal
    <>
    flag1 "-J" joinWrapped
    <>
    flag1 "-P" incomplete
    <>
    optionArgs output
    <>
    optionArgsWith "-S" startLine
    <>
    optionArgsWith "-E" endLine
    <>
    cmdArgs target
