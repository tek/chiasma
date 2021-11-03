module Chiasma.Data.SplitParams where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), flag1, optionArgs)
import Chiasma.Data.Axis (Axis)
import Chiasma.Data.Target (Target (Current), formatTarget)

data SplitSize =
  Units Int
  |
  Percentage Int
  deriving stock (Eq, Show)

instance CmdArgs SplitSize where
  cmdArgs = \case
    Units n -> ["-l", show n]
    Percentage n -> ["-p", show n]

data SplitWindowParams =
  SplitWindowParams {
    axis :: Maybe Axis,
    size :: Maybe SplitSize,
    before :: Bool,
    fullSize :: Bool,
    readStdin :: Bool
  }
  deriving stock (Eq, Show)

instance Default SplitWindowParams where
  def =
    SplitWindowParams {
      axis = Nothing,
      size = Nothing,
      before = False,
      fullSize = False,
      readStdin = False
    }

instance CmdArgs SplitWindowParams where
  cmdArgs SplitWindowParams {..} =
    optionArgs axis
    <>
    optionArgs size
    <>
    flag1 "-b" before
    <>
    flag1 "-f" fullSize
    <>
    flag1 "-K" readStdin

data JoinPaneParams =
  JoinPaneParams {
    axis :: Maybe Axis,
    size :: Maybe SplitSize,
    before :: Bool,
    detach :: Bool,
    source :: Maybe Target,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default JoinPaneParams where
  def =
    JoinPaneParams {
      axis = Nothing,
      size = Nothing,
      before = False,
      detach = False,
      source = Nothing,
      target = Current
    }

instance CmdArgs JoinPaneParams where
  cmdArgs JoinPaneParams {..} =
    optionArgs axis
    <>
    optionArgs size
    <>
    flag1 "-b" before
    <>
    flag1 "-d" detach
    <>
    foldMap (formatTarget ["-s"]) source
    <>
    cmdArgs target
