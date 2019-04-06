{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Data.Ident where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.UUID (UUID, toString)
import GHC.Generics (Generic)
import System.Random (randomIO)

data Ident =
  Str String
  |
  Uuid UUID
  deriving (Eq, Show, Generic, Data, NFData, Ord)

class Identifiable a where
  identify :: a -> Ident

instance Pretty Ident where
  pretty (Str s) = pretty s
  pretty (Uuid u) = pretty . toString $ u

sameIdent :: Identifiable a => Ident -> a -> Bool
sameIdent target a =
  target == identify a

identString :: Ident -> String
identString (Str a) = a
identString (Uuid a) = toString a

generateIdent :: MonadIO m => m Ident
generateIdent = liftIO $ Uuid <$> randomIO
