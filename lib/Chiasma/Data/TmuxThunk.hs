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

import Chiasma.Data.Cmd (CmdName(..), CmdArgs(..), Cmd(..), Cmds(..), cmd)
import Chiasma.Data.TmuxError (TmuxError(..))
import Chiasma.Codec.Decode (TmuxDecodeError)

data TmuxThunk next =
  ∀ a . Read Cmd ([String] -> Either TmuxDecodeError a) ([a] -> next)
  |
  Write Cmd (() -> next)
  |
  Failed TmuxError

deriving instance Functor TmuxThunk
