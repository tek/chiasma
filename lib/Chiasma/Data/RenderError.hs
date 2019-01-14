module Chiasma.Data.RenderError(
  RenderError(..),
) where

import Chiasma.Data.Views (ViewsError)
import Chiasma.Data.PackError (PackError)

data RenderError =
  RenderError String
  |
  Views ViewsError
  |
  Pack PackError
  deriving (Eq, Show)
