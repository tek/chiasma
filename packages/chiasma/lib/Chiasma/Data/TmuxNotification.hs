module Chiasma.Data.TmuxNotification where

data TmuxNotification =
  TmuxNotification {
    name :: Text,
    args :: [Text]
  }
  deriving stock (Eq, Show, Ord)
