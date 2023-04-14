module Chiasma.Data.View where

import qualified Control.Lens as Lens (set)

import Chiasma.Data.Ident (Ident, Identifiable (..))

data View a =
  View {
    ident :: Ident,
    id :: Maybe a
  }
  deriving stock (Eq, Show, Generic)

instance Identifiable (View a) where
  identify = (.ident)

setViewId :: a -> View a -> View a
setViewId =
  Lens.set #id . Just
