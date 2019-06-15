module Chiasma.Data.TmuxId where

import Data.Text.Prettyprint.Doc (Pretty(..))
import GHC.Generics (Generic)

sessionPrefix :: Char
sessionPrefix = '$'

newtype SessionId =
  SessionId Int
  deriving (Eq, Show, Generic, Num)

instance Pretty SessionId where
  pretty = pretty . formatId

windowPrefix :: Char
windowPrefix = '@'

newtype WindowId =
  WindowId Int
  deriving (Eq, Show, Generic, Num)

instance Pretty WindowId where
  pretty = pretty . formatId

panePrefix :: Char
panePrefix = '%'

newtype PaneId =
  PaneId Int
  deriving (Eq, Show, Generic, Num)

instance Pretty PaneId where
  pretty = pretty . formatId

class HasPaneId a where
  paneId :: a -> PaneId

newtype TmuxIdPrefix a =
  TmuxIdPrefix Char

class TmuxId a where
  prefix :: TmuxIdPrefix a
  number :: a -> Int

  formatId :: a -> String
  formatId a =
    let (TmuxIdPrefix p) = prefix @a
    in p : show (number a)

instance TmuxId SessionId where
  prefix = TmuxIdPrefix sessionPrefix
  number (SessionId i) = i

instance TmuxId WindowId where
  prefix = TmuxIdPrefix windowPrefix
  number (WindowId i) = i

instance TmuxId PaneId where
  prefix = TmuxIdPrefix panePrefix
  number (PaneId i) = i
