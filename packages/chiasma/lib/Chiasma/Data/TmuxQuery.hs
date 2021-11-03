module Chiasma.Data.TmuxQuery where

newtype TmuxQuery =
  TmuxQuery { unTmuxQuery :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Semigroup, Monoid)
