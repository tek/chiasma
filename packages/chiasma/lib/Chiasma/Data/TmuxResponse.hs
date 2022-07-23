module Chiasma.Data.TmuxResponse where

newtype TmuxResponse =
  TmuxResponse { unTmuxResponse :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)
