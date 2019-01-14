{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chiasma.Data.View(
  View(..),
  setViewId,
) where

import qualified Control.Lens as Lens (set)
import Control.Lens (makeClassy_)
import Chiasma.Data.Ident (Ident, Identifiable(..))

data View a =
  View {
    viewIdent :: Ident,
    viewId :: Maybe a
  }
  deriving (Eq, Show)

makeClassy_ ''View

instance Identifiable (View a) where
  identify = viewIdent

setViewId :: a -> View a -> View a
setViewId =
  Lens.set _viewId . Just
