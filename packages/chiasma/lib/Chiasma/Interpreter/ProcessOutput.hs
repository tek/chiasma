module Chiasma.Interpreter.ProcessOutput where

import Data.Attoparsec.ByteString (parse)
import Data.Attoparsec.Types (IResult (Done, Fail, Partial))
import Polysemy.Conc (interpretAtomic)
import Polysemy.Process.Effect.ProcessOutput (ProcessOutput (Chunk))

import Chiasma.Data.TmuxEvent (TmuxEvent)
import Chiasma.Native.TmuxOutputBlock (messageParser)

type ParseResult =
  IResult ByteString TmuxEvent

type ParseCont =
  ByteString -> IResult ByteString TmuxEvent

parseResult ::
  Member (AtomicState (Maybe ParseCont)) r =>
  ParseResult ->
  Sem r ([Either Text TmuxEvent], ByteString)
parseResult = \case
  Fail _ _ err -> pure ([Left (toText err)], "")
  Partial c -> ([], "") <$ atomicPut (Just c)
  Done rest block -> pure ([Right block], rest)

interpretProcessOutputTmuxEvent ::
  ∀ p r .
  Member (Embed IO) r =>
  InterpreterFor (ProcessOutput p (Either Text TmuxEvent)) r
interpretProcessOutputTmuxEvent =
  interpretAtomic (Nothing :: Maybe ParseCont) .
  reinterpret \case
    Chunk _ new ->
      atomicState' (Nothing,) >>= \case
        Just cont ->
          parseResult (cont new)
        Nothing ->
          parseResult (parse messageParser new)
