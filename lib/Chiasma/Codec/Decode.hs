{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Chiasma.Codec.Decode(
  TmuxDecodeError(..),
  TmuxPrimDecode(..),
  TmuxDataDecode(..),
  idParser,
  parseId,
  readInt,
) where

import GHC.Generics ((:*:)(..), D1, C1, S1, K1(..), M1(..))
import Data.Bifunctor (first)
import Text.Read (readEither)
import Text.ParserCombinators.Parsec (
  GenParser,
  ParseError,
  parse,
  many,
  )
import Text.Parsec.Char (char, digit)

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
  decode' as = do
    (rest, left) <- decode' as
    (rest1, right) <- decode' rest
    return (rest1, left :*: right)

instance TmuxDataDecode f => (TmuxDataDecode (D1 c f)) where
  decode' fields = do
    (rest, sub) <- decode' fields
    return (rest, M1 sub)

instance TmuxDataDecode a => (TmuxDataDecode (C1 c a)) where
  decode' fields = do
    (rest, sub) <- decode' fields
    return (rest, M1 sub)

instance TmuxDataDecode a => (TmuxDataDecode (S1 c a)) where
  decode' fields = do
    (rest, sub) <- decode' fields
    return (rest, M1 sub)

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
