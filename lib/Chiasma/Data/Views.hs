{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Data.Views where

import Data.DeepLenses (deepLenses)
import Data.DeepPrisms (deepPrisms)
import Data.Default.Class (Default)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import GHC.Generics (Generic)
import Prelude hiding (log)

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId, SessionId, WindowId)
import Chiasma.Data.View (View)

data ViewsError =
  NoSuchSession Ident
  |
  NoSuchWindow Ident
  |
  NoSuchPane Ident
  |
  NoPaneId Ident
  deriving (Eq, Show)

deepPrisms ''ViewsError

data Views =
  Views {
    _sessions :: [View SessionId],
    _windows :: [View WindowId],
    _panes :: [View PaneId],
    _log :: [Doc AnsiStyle]
  }
  deriving (Show, Generic, Default)

instance Eq Views where
  (Views sa wa pa _) == (Views sb wb pb _) = (sa == sb) && (wa == wb) && (pa == pb)

deepLenses ''Views
