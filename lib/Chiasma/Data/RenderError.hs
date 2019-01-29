module Chiasma.Data.RenderError(
  RenderError(..),
) where

import Chiasma.Data.Views (ViewsError)
import Chiasma.Data.TmuxThunk (TmuxError)

data RenderError =
  RenderError String
  |
  Views ViewsError
  |
  Pack String
  |
  Fatal TmuxError
  deriving (Eq, Show)
