module Chiasma.Data.Cmd(
  CmdName(..),
  CmdArgs(..),
  Cmd(..),
  Cmds(..),
  cmd,
) where

newtype CmdName =
  CmdName String
  deriving (Eq, Show)

newtype CmdArgs =
  CmdArgs [String]
  deriving (Eq, Show)

data Cmd =
  Cmd CmdName CmdArgs
  deriving (Eq, Show)

newtype Cmds =
  Cmds [Cmd]
  deriving (Eq, Show)

cmd :: String -> [String] -> Cmd
cmd name args = Cmd (CmdName name) (CmdArgs args)
