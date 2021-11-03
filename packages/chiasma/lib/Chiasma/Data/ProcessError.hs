module Chiasma.Data.ProcessError where

data ProcessError =
  Terminated Text
  deriving stock (Eq, Show)
