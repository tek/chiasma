module Chiasma.Data.View where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Control.Lens (makeClassy_)
import qualified Control.Lens as Lens (set)

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
