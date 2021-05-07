module Chiasma.Ui.Data.ViewState where

import Data.Data (Data)

newtype ViewState =
  ViewState {
    minimized :: Bool
  }
  deriving (Eq, Show, Data, Generic)

instance Default ViewState where
  def = ViewState False
