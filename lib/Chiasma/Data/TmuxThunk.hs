{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import Chiasma.Codec.Decode (TmuxDecodeError)

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

data TmuxError =
  ProcessFailed Cmds String
  |
  OutputParsingFailed Cmds [String] ParseError
  |
  NoOutput Cmds
  |
  DecodingFailed Cmds String TmuxDecodeError
  |
  InvalidOutput String String
  |
  Other String
  deriving (Eq, Show)

data TmuxThunk next =
  ∀ a . Read Cmd ([String] -> Either TmuxDecodeError a) ([a] -> next)
  |
  Write Cmd (() -> next)
  |
  Failed TmuxError

deriving instance Functor TmuxThunk

cmd :: String -> [String] -> Cmd
cmd name args = Cmd (CmdName name) (CmdArgs args)
