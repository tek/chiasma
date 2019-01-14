{-# LANGUAGE DeriveDataTypeable #-}

module Chiasma.Ui.Data.ViewState(
  ViewState(..),
) where

import GHC.Generics (Generic)
import Data.Data (Data)

newtype ViewState =
  ViewState {
    minimized :: Bool
  }
  deriving (Eq, Show, Data, Generic)
