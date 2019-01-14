{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chiasma.Data.Views(
  Views(..),
  _viewsSessions,
  _viewsWindows,
  _viewsPanes,
  ViewsError(..),
) where

import Control.Lens (makeClassy_)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.View (View)
import Chiasma.Data.TmuxId (SessionId, WindowId, PaneId)

data ViewsError =
  NoSuchSession Ident
  |
  NoSuchWindow Ident
  |
  NoSuchPane Ident
  deriving (Eq, Show)

data Views =
  Views {
    viewsSessions :: [View SessionId],
    viewsWindows :: [View WindowId],
    viewsPanes :: [View PaneId]
  }
  deriving (Eq, Show)

makeClassy_ ''Views
