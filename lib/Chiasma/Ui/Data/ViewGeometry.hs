{-# LANGUAGE DeriveDataTypeable #-}

module Chiasma.Ui.Data.ViewGeometry(
  ViewGeometry(..),
) where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Default.Class (Default)

data ViewGeometry =
  ViewGeometry {
    minSize :: Maybe Float,
    maxSize :: Maybe Float,
    fixedSize :: Maybe Float,
    minimizedSize :: Maybe Float,
    weight :: Maybe Float,
    position :: Maybe Float
  }
  deriving (Eq, Show, Data, Generic, Default)
