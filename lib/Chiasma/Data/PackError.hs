module Chiasma.Data.PackError(
  PackError(..),
) where

newtype PackError =
  PackError String
  deriving (Eq, Show)
