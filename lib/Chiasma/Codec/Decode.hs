{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Chiasma.Codec.Decode(
  TmuxDecodeError(..),
  TmuxPrimDecode(..),
  TmuxDataDecode(..),
  idParser,
  parseId,
  readInt,
) where

import Data.Bifunctor (first, second)
import GHC.Generics ((:*:)(..), K1(..), M1(..))
import Text.ParserCombinators.Parsec (
  GenParser,
  ParseError,
  parse,
  many,
  )
import Text.Parsec.Char (char, digit)
import Text.Read (readEither)

import Chiasma.Data.TmuxId (SessionId(..), WindowId(..), PaneId(..), sessionPrefix, windowPrefix, panePrefix)

data TmuxDecodeError =
  ParseFailure String ParseError
  |
  IntParsingFailure String
  |
  TooFewFields
  |
  TooManyFields [String]
  deriving (Eq, Show)

class TmuxPrimDecode a where
  primDecode :: String -> Either TmuxDecodeError a

class TmuxDataDecode f where
  decode' :: [String] -> Either TmuxDecodeError ([String], f a)

instance (TmuxDataDecode f, TmuxDataDecode g) => TmuxDataDecode (f :*: g) where
  decode' fields = do
    (rest, left) <- decode' fields
    (rest1, right) <- decode' rest
    return (rest1, left :*: right)

instance TmuxDataDecode f => (TmuxDataDecode (M1 i c f)) where
  decode' fields =
    second M1 <$> decode' fields

instance TmuxPrimDecode a => (TmuxDataDecode (K1 c a)) where
  decode' (a:as) = do
    prim <- primDecode a
    return (as, K1 prim)
  decode' [] = Left TooFewFields

readInt :: String -> String -> Either TmuxDecodeError Int
readInt input num =
  first (const $ IntParsingFailure input) $ readEither num

instance TmuxPrimDecode Int where
  primDecode field = readInt field field

idParser :: Char -> GenParser Char st String
idParser sym = char sym >> many digit

parseId :: (Int -> a) -> Char -> String -> Either TmuxDecodeError a
parseId cons sym input = do
  num <- first (ParseFailure "id") $ parse (idParser sym) "none" input
  i <- readInt input num
  return $ cons i

instance TmuxPrimDecode SessionId where
  primDecode = parseId SessionId sessionPrefix

instance TmuxPrimDecode WindowId where
  primDecode = parseId WindowId windowPrefix

instance TmuxPrimDecode PaneId where
  primDecode = parseId PaneId panePrefix

instance TmuxPrimDecode [Char] where
  primDecode = Right
