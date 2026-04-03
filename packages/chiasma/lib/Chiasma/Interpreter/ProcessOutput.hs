module Chiasma.Interpreter.ProcessOutput where

import Data.Attoparsec.ByteString (parse)
import qualified Data.Attoparsec.Types as Attoparsec
import qualified Polysemy.Process.Data.ProcessOutputParseResult as ProcessOutputParseResult
import Polysemy.Process.Data.ProcessOutputParseResult (ProcessOutputParseResult)
import Polysemy.Process.Effect.ProcessOutput (ProcessOutput)
import Polysemy.Process.Interpreter.ProcessOutput (interpretProcessOutputIncremental)

import Chiasma.Data.TmuxEvent (TmuxEvent)
import Chiasma.Native.TmuxOutputBlock (messageParser)

-- | Convert an attoparsec 'Attoparsec.IResult' to 'ProcessOutputParseResult'.
fromAttoparsec :: Attoparsec.IResult ByteString TmuxEvent -> ProcessOutputParseResult TmuxEvent
fromAttoparsec = \case
  Attoparsec.Fail _ _ err -> ProcessOutputParseResult.Fail (toText err)
  Attoparsec.Partial c -> ProcessOutputParseResult.Partial (fromAttoparsec . c)
  Attoparsec.Done rest a -> ProcessOutputParseResult.Done a rest

interpretProcessOutputTmuxEvent ::
  ∀ p r .
  InterpreterFor (ProcessOutput p (Either Text TmuxEvent)) r
interpretProcessOutputTmuxEvent =
  interpretProcessOutputIncremental (fromAttoparsec . parse messageParser)
