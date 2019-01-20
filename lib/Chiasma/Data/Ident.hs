{-# LANGUAGE DeriveDataTypeable #-}

module Chiasma.Data.Ident(
  Ident(..),
  Identifiable(..),
  sameIdent,
  identString,
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.UUID (UUID, toString)
import Data.Data (Data)

data Ident =
  Str String
  |
  Uuid UUID
  deriving (Eq, Show, Generic, Data, NFData)

class Identifiable a where
  identify :: a -> Ident

sameIdent :: Identifiable a => Ident -> a -> Bool
sameIdent target a =
  target == identify a

identString :: Ident -> String
identString (Str a) = a
identString (Uuid a) = toString a
