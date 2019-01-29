module Chiasma.Data.TmuxError(
  TmuxError(..),
) where

import Text.ParserCombinators.Parsec (ParseError)
import Chiasma.Codec.Decode (TmuxDecodeError)
import Chiasma.Data.Cmd (Cmds(..))

data TmuxError =
  ProcessFailed {
    processFailedCmds :: Cmds,
    processFailedReason :: String
    }
  |
  OutputParsingFailed {
    parsingFailedCmds :: Cmds,
    parsingFailedOutput :: [String],
    parsingFailedError :: ParseError
  }
  |
  NoOutput Cmds
  |
  DecodingFailed {
    decodingFailedCmds :: Cmds,
    decodingFailedOutput :: String,
    decodingFailedError :: TmuxDecodeError
  }
  |
  InvalidOutput {
    invalidOutputReason :: String,
    invalidOutputCommand :: String
  }
  deriving (Eq, Show)
