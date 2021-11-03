module Chiasma.Data.LayoutError where

newtype LayoutError =
  LayoutError { unLayoutError :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)
