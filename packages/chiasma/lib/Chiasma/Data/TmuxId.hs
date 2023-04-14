module Chiasma.Data.TmuxId where

import Prettyprinter (Pretty (..))

newtype ClientId =
  ClientId { unClientId :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

instance Pretty ClientId where
  pretty = pretty . (.unClientId)

sessionPrefix :: Text
sessionPrefix = "$"

newtype SessionId =
  SessionId Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord)

instance Pretty SessionId where
  pretty = pretty . formatId

windowPrefix :: Text
windowPrefix = "@"

newtype WindowId =
  WindowId Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord)

instance Pretty WindowId where
  pretty = pretty . formatId

panePrefix :: Text
panePrefix = "%"

newtype PaneId =
  PaneId Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord)

instance Pretty PaneId where
  pretty = pretty . formatId

class HasPaneId a where
  paneId :: a -> PaneId

newtype TmuxIdPrefix a =
  TmuxIdPrefix { unTmuxIdPrefix :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

class TmuxId a where
  prefix :: TmuxIdPrefix a
  number :: a -> Int

  formatId :: a -> Text
  formatId a =
    p <> show (number a)
    where
      (TmuxIdPrefix p) = prefix @a

instance TmuxId SessionId where
  prefix = TmuxIdPrefix sessionPrefix
  number (SessionId i) = i

instance TmuxId WindowId where
  prefix = TmuxIdPrefix windowPrefix
  number (WindowId i) = i

instance TmuxId PaneId where
  prefix = TmuxIdPrefix panePrefix
  number (PaneId i) = i
