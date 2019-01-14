module Chiasma.Data.TmuxId(
  SessionId(..),
  WindowId(..),
  PaneId(..),
  TmuxId(..),
  sessionPrefix,
  windowPrefix,
  panePrefix,
) where

sessionPrefix :: Char
sessionPrefix = '$'

newtype SessionId =
  SessionId Int
  deriving (Eq, Show)

windowPrefix :: Char
windowPrefix = '@'

newtype WindowId =
  WindowId Int
  deriving (Eq, Show)

panePrefix :: Char
panePrefix = '%'

newtype PaneId =
  PaneId Int
  deriving (Eq, Show)

data TmuxIdPrefix a =
  TmuxIdPrefix Char

class TmuxId a where
  prefix :: TmuxIdPrefix a
  number :: a -> Int

  formatId :: a -> String
  formatId a =
    let (TmuxIdPrefix p) = prefix @a
    in p : (show $ number a)

instance TmuxId SessionId where
  prefix = TmuxIdPrefix sessionPrefix
  number (SessionId i) = i

instance TmuxId WindowId where
  prefix = TmuxIdPrefix windowPrefix
  number (WindowId i) = i

instance TmuxId PaneId where
  prefix = TmuxIdPrefix panePrefix
  number (PaneId i) = i
