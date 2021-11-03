module Chiasma.Data.TmuxOutputBlock where

data End =
  EndSuccess
  |
  EndError
  deriving stock (Eq, Show)

data TmuxOutputBlock =
  Success [Text]
  |
  Error [Text]
  deriving stock (Eq, Show)
