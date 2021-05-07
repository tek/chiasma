module Chiasma.Data.TmuxError where

import Chiasma.Codec.Decode (TmuxDecodeError)
import Text.ParserCombinators.Parsec (ParseError)

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
    invalidOutputReason :: Text,
    invalidOutputCommand :: Text
  }
  |
  CommandFailed {
    commandFailedCmds :: Cmds,
    commandFailedError :: [Text]
  }
  deriving (Eq, Show)

deepPrisms ''TmuxError
