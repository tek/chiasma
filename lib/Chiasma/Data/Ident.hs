{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Data.Ident where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Default.Class (Default(def))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text (pack)
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

instance Identifiable Ident where
  identify = id

instance Pretty Ident where
  pretty (Str s) = pretty s
  pretty (Uuid u) = pretty . toString $ u

instance Default Ident where
  def = Str def

instance IsString Ident where
  fromString = Str

sameIdent ::
  Identifiable a =>
  Identifiable b =>
  a ->
  b ->
  Bool
sameIdent target b =
  identify target == identify b

identString :: Ident -> String
identString (Str a) = a
identString (Uuid a) = toString a

identText :: Ident -> Text
identText (Str a) = Text.pack a
identText (Uuid a) = Text.pack $ toString a

generateIdent :: MonadIO m => m Ident
generateIdent = liftIO $ Uuid <$> randomIO
