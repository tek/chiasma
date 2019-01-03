{-# LANGUAGE DeriveFunctor #-}

module Chiasma.Data.TmuxThunk(
  CmdName(..),
  CmdArgs(..),
  TmuxThunk(..),
  TmuxError(..),
  Cmd(..),
  Cmds(..),
  cmd,
) where

import Text.ParserCombinators.Parsec (ParseError)

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

data TmuxThunk a next =
  Read Cmd (String -> Either TmuxError a) ([a] -> next)
  |
  Write Cmd (() -> next)
  deriving Functor

data TmuxError =
  TmuxProcessFailed Cmds String
  |
  TmuxOutputParsingFailed Cmds String ParseError
  |
  TmuxNoOutput Cmds
  |
  TmuxDecodingFailed Cmds String String
  deriving (Eq, Show)

cmd :: String -> [String] -> Cmd
cmd name args = Cmd (CmdName name) (CmdArgs args)
