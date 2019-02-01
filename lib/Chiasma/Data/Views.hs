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

import GHC.Generics (Generic)
import Control.Lens (makeClassy_)
import Data.Default.Class (Default)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.View (View)
import Chiasma.Data.TmuxId (SessionId, WindowId, PaneId)

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
    viewsLog :: [String]
  }
  deriving (Eq, Show, Generic, Default)

makeClassy_ ''Views
