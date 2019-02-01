{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Data.Ident(
  Ident(..),
  Identifiable(..),
  sameIdent,
  identString,
  generateIdent,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import System.Random (randomIO)
import Data.UUID (UUID, toString)
import Data.Data (Data)

data Ident =
  Str String
  |
  Uuid UUID
  deriving (Eq, Show, Generic, Data, NFData, Ord)

class Identifiable a where
  identify :: a -> Ident

sameIdent :: Identifiable a => Ident -> a -> Bool
sameIdent target a =
  target == identify a

identString :: Ident -> String
identString (Str a) = a
identString (Uuid a) = toString a

generateIdent :: MonadIO m => m Ident
generateIdent = liftIO $ Uuid <$> randomIO
