module Chiasma.Data.Ident where

import Data.Data (Data)
import Data.UUID (UUID)
import qualified Data.UUID as UUID (fromText, toText)
import Prettyprinter (Pretty (..))
import System.Random (randomIO)

data Ident =
  Str Text
  |
  Uuid UUID
  deriving stock (Eq, Show, Ord, Generic, Data)

json ''Ident

class Identifiable a where
  identify :: a -> Ident

instance Identifiable Ident where
  identify = id

instance Pretty Ident where
  pretty =
    pretty . \case
      Str s -> s
      Uuid u -> UUID.toText u

instance Default Ident where
  def =
    Str ""

instance IsString Ident where
  fromString =
    Str . toText

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
