{-# LANGUAGE TemplateHaskell #-}

module Chiasma.Data.TmuxError where

import Data.DeepPrisms (deepPrisms)
import Data.Text (Text)
import Text.ParserCombinators.Parsec (ParseError)

import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmds(..))

data TmuxError =
  ProcessFailed {
    processFailedCmds :: Cmds,
    processFailedReason :: Text
    }
  |
  OutputParsingFailed {
    parsingFailedCmds :: Cmds,
    parsingFailedOutput :: [Text],
    parsingFailedError :: ParseError
  }
  |
  NoOutput Cmds
  |
  DecodingFailed {
    decodingFailedCmds :: Cmds,
    decodingFailedOutput :: Text,
    decodingFailedError :: TmuxDecodeError
  }
  |
  InvalidOutput {
    invalidOutputReason :: String,
    invalidOutputCommand :: String
  }
  |
  CommandFailed {
    commandFailedCmds :: Cmds,
    commandFailedError :: [Text]
  }
  deriving (Eq, Show)

deepPrisms ''TmuxError
