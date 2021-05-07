module Chiasma.Data.RenderError where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)

data RenderError =
  NoPrincipal Ident
  |
  Views ViewsError
  |
  Pack Text
  |
  Fatal TmuxError
  deriving (Eq, Show)

deepPrisms ''RenderError
