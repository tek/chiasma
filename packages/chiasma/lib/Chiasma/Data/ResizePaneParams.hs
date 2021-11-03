module Chiasma.Data.ResizePaneParams where

import Chiasma.Class.CmdArgs (CmdArgs (cmdArgs), arg, flag1, optionArgs)
import Chiasma.Data.Axis (Axis (Horizontal, Vertical))
import Chiasma.Data.Direction (Direction)
import Chiasma.Data.Target (Target (Current))

data ResizeAbsolute =
  ResizeAbsolute Axis Int
  deriving stock (Eq, Show)

instance CmdArgs ResizeAbsolute where
  cmdArgs (ResizeAbsolute axis amount) =
    [axisArg axis, show amount]
    where
      axisArg = \case
        Horizontal -> "-x"
        Vertical -> "-y"

data Adjustment =
  Adjustment (Maybe Direction) (Maybe Int)
  deriving stock (Eq, Show)

instance CmdArgs Adjustment where
  cmdArgs (Adjustment dir amount) =
    optionArgs dir <> arg (show <$> amount)

data ResizePaneParams =
  ResizePaneParams {
    absolute :: Maybe ResizeAbsolute,
    adjustment :: Maybe Adjustment,
    zoom :: Bool,
    mouse :: Bool,
    target :: Target
  }
  deriving stock (Eq, Show)

instance Default ResizePaneParams where
  def =
    ResizePaneParams {
      absolute = Nothing,
      adjustment = Nothing,
      zoom = False,
      mouse = False,
      target = Current
    }

instance CmdArgs ResizePaneParams where
  cmdArgs ResizePaneParams {..} =
    optionArgs absolute
    <>
    flag1 "-Z" zoom
    <>
    flag1 "-M" mouse
    <>
    optionArgs adjustment
    <>
    cmdArgs target
