{-# LANGUAGE DeriveFunctor #-}

module Chiasma.Data.TmuxThunk(
  Cmd(..),
  Args(..),
  TmuxThunk(..),
  TmuxCommandFailed(..),
) where

import GHC.Exception.Type (Exception)

newtype Cmd =
  Cmd String
  deriving (Eq, Show)

newtype Args =
  Args [String]
  deriving (Eq, Show)

data TmuxThunk a next =
  Read Cmd Args ([String] -> next)
  |
  Write Cmd Args (() -> next)
  deriving Functor

data TmuxCommandFailed =
  TmuxCommandFailed Cmd Args [String]
  deriving Show

instance Exception TmuxCommandFailed
