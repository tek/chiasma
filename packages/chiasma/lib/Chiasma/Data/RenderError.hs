module Chiasma.Data.RenderError where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.LayoutError (LayoutError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)

data RenderError =
  NoPrincipal Ident
  |
  Views ViewsError
  |
  Layout LayoutError
  |
  Pack Text
  |
  Fatal TmuxError
  |
  Codec CodecError
  deriving stock (Eq, Show)
