{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Ui.Data.ViewGeometry where

import Data.Data (Data)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), emptyDoc, space, (<+>))

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

mayPretty :: Text -> Maybe Float -> Doc a
mayPretty prefix (Just a) =
  space <> pretty prefix <> ":" <+> pretty a
mayPretty _ Nothing =
  emptyDoc

instance Pretty ViewGeometry where
  pretty (ViewGeometry minSize maxSize fixedSize _ _ _) =
    foldl (<>) emptyDoc (uncurry mayPretty <$> [("min", minSize), ("max", maxSize), ("fixed", fixedSize)])
