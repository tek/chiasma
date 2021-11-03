module Chiasma.Data.SessionGroupId where

newtype SessionGroupId =
  SessionGroupId { unSessionGroupId :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)
