{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Data.Views(
  Views(..),
  _viewsSessions,
  _viewsWindows,
  _viewsPanes,
  _viewsLog,
  ViewsError(..),
) where

import Control.Lens (makeClassy_)
import Data.Default.Class (Default)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import GHC.Generics (Generic)

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (SessionId, WindowId, PaneId)
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

data Views =
  Views {
    viewsSessions :: [View SessionId],
    viewsWindows :: [View WindowId],
    viewsPanes :: [View PaneId],
    viewsLog :: [Doc AnsiStyle]
  }
  deriving (Show, Generic, Default)

instance Eq Views where
  (Views sa wa pa _) == (Views sb wb pb _) = (sa == sb) && (wa == wb) && (pa == pb)

makeClassy_ ''Views
