{-# LANGUAGE DeriveAnyClass #-}

module Chiasma.Data.Ident where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON(toEncoding), defaultOptions, genericToEncoding)
import Data.Data (Data)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.UUID (UUID)
import qualified Data.UUID as UUID (fromText, toText, toText)
import System.Random (randomIO)
import qualified Text.Show as Show

data Ident =
  Str Text
  |
  Uuid UUID
  deriving (Eq, Generic, Data, NFData, Ord)

instance Show Ident where
  show = toString . identText

class Identifiable a where
  identify :: a -> Ident

instance Identifiable Ident where
  identify = id

instance Pretty Ident where
  pretty (Str s) = pretty s
  pretty (Uuid u) = pretty . UUID.toText $ u

instance Default Ident where
  def = Str ""

instance IsString Ident where
  fromString = Str . toText

instance FromJSON Ident where

instance ToJSON Ident where
  toEncoding = genericToEncoding defaultOptions

sameIdent ::
  Identifiable a =>
  Identifiable b =>
  a ->
  b ->
  Bool
sameIdent target b =
  identify target == identify b

identText :: Ident -> Text
identText (Str a) = a
identText (Uuid a) = UUID.toText a

generateIdent :: MonadIO m => m Ident
generateIdent = liftIO $ Uuid <$> randomIO

parseIdent :: Text -> Ident
parseIdent text =
  maybe (Str text) Uuid (UUID.fromText text)
