module Chiasma.Codec.Decode where

import qualified Data.Text as Text (null, unpack)
import Data.Text.Read (decimal)
import GHC.Generics (K1 (..), M1 (..), (:*:) (..))
import Prelude hiding (many)
import Text.Parsec.Char (digit, string)
import Text.ParserCombinators.Parsec (GenParser, many, parse)

import Chiasma.Data.DecodeError (DecodeFailure (BoolParsingFailure, IntParsingFailure, ParseFailure, TooFewFields))
import Chiasma.Data.TmuxId (
  ClientId (ClientId),
  PaneId (..),
  SessionId (..),
  WindowId (..),
  panePrefix,
  sessionPrefix,
  windowPrefix,
  )

class TmuxPrimDecode a where
  primDecode :: Text -> Either DecodeFailure a

class TmuxDataDecode f where
  decode' :: [Text] -> Either DecodeFailure ([Text], f a)

instance (TmuxDataDecode f, TmuxDataDecode g) => TmuxDataDecode (f :*: g) where
  decode' fields = do
    (rest, left) <- decode' fields
    (rest1, right) <- decode' rest
    pure (rest1, left :*: right)

instance TmuxDataDecode f => (TmuxDataDecode (M1 i c f)) where
  decode' fields =
    second M1 <$> decode' fields

instance TmuxPrimDecode a => (TmuxDataDecode (K1 c a)) where
  decode' (a:as') = do
    prim <- primDecode a
    pure (as', K1 prim)
  decode' [] = Left TooFewFields

readInt :: Text -> Text -> Either DecodeFailure Int
readInt text num =
  first (const $ IntParsingFailure text) parsed
  where
    parsed = do
      (num', rest) <- decimal num
      if Text.null rest then Right num' else Left ""

instance TmuxPrimDecode Int where
  primDecode field = readInt field field

instance TmuxPrimDecode Bool where
  primDecode field =
    convert =<< readInt field field
    where
      convert 0 =
        Right False
      convert 1 =
        Right True
      convert _ =
        Left (BoolParsingFailure $ "got non-bool `" <> show field <> "`")

idParser :: Text -> GenParser Char st Text
idParser sym =
  string (toString sym) *> (toText <$> many digit)

parseId :: (Int -> a) -> Text -> Text -> Either DecodeFailure a
parseId cons sym text = do
  num <- first (ParseFailure "id") $ parse (idParser sym) "none" (Text.unpack text)
  i <- readInt text num
  pure (cons i)

instance TmuxPrimDecode ClientId where
  primDecode = pure . ClientId

instance TmuxPrimDecode SessionId where
  primDecode = parseId SessionId sessionPrefix

instance TmuxPrimDecode WindowId where
  primDecode = parseId WindowId windowPrefix

instance TmuxPrimDecode PaneId where
  primDecode = parseId PaneId panePrefix

instance TmuxPrimDecode [Char] where
  primDecode = Right . toString

instance TmuxPrimDecode Text where
  primDecode = Right
