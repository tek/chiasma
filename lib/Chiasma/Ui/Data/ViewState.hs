{-# LANGUAGE DeriveDataTypeable #-}

module Chiasma.Ui.Data.ViewState where

import Data.Data (Data)
import Data.Default.Class (Default(def))
import GHC.Generics (Generic)

newtype ViewState =
  ViewState {
    minimized :: Bool
  }
  deriving (Eq, Show, Data, Generic)

instance Default ViewState where
  def = ViewState False
