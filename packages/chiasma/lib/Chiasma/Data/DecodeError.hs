module Chiasma.Data.DecodeError where

import Text.ParserCombinators.Parsec (ParseError)

data DecodeFailure =
  ParseFailure Text ParseError
  |
  IntParsingFailure Text
  |
  BoolParsingFailure Text
  |
  TooFewFields
  |
  TooManyFields [Text]
  |
  TooManyRecords [Text]
  |
  TargetMissing
  deriving stock (Eq, Show)

data DecodeError =
  DecodeError {
    output :: [Text],
    failure :: DecodeFailure
  }
  deriving stock (Eq, Show)
