module Chiasma.Interpreter.ProcessOutput where

import Data.Attoparsec.ByteString (parse)
import Data.Attoparsec.Types (IResult (Done, Fail, Partial))
import Polysemy.Conc (interpretAtomic)
import Polysemy.Process.Effect.ProcessOutput (ProcessOutput (Chunk))

import Chiasma.Data.TmuxOutputBlock (TmuxOutputBlock)
import Chiasma.Native.TmuxOutputBlock (parser)

type ParseResult =
  IResult ByteString TmuxOutputBlock

type ParseCont =
  ByteString -> IResult ByteString TmuxOutputBlock

parseResult ::
  Member (AtomicState (Maybe ParseCont)) r =>
  ParseResult ->
  Sem r ([Either Text TmuxOutputBlock], ByteString)
parseResult = \case
  Fail _ _ err -> pure ([Left (toText err)], "")
  Partial c -> ([], "") <$ atomicPut (Just c)
  Done rest block -> pure ([Right block], rest)

interpretProcessOutputTmuxBlock ::
  Member (Embed IO) r =>
  InterpreterFor (ProcessOutput (Either Text TmuxOutputBlock)) r
interpretProcessOutputTmuxBlock =
  interpretAtomic (Nothing :: Maybe ParseCont) .
  interpret \case
    Chunk _ new ->
      atomicState' (Nothing,) >>= \case
        Just cont ->
          parseResult (cont new)
        Nothing ->
          parseResult (parse parser new)
  . raiseUnder
