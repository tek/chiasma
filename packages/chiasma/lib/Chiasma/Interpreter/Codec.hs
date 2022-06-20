module Chiasma.Interpreter.Codec where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.CodecError (CodecError (CodecError))
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (Panes)
import qualified Chiasma.Data.TmuxCommand as TmuxCommand
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import qualified Chiasma.Effect.Codec as Codec
import Chiasma.Effect.Codec (Codec)

interpretCodecTmuxCommand ::
  InterpreterFor (Codec TmuxCommand (Const TmuxRequest) (Const [Text]) !! CodecError) r
interpretCodecTmuxCommand =
  interpretResumable \case
    Codec.Encode cmd ->
      pure (Const (TmuxCommand.encode cmd))
    Codec.Decode cmd (Const req) (Const out) ->
      stopEitherWith (CodecError req) (TmuxCommand.decode out cmd)
{-# inline interpretCodecTmuxCommand #-}

interpretCodecPanes ::
  TmuxCodec p =>
  InterpreterFor (Codec (Panes p) (Const TmuxRequest) (Const [Text]) !! CodecError) r
interpretCodecPanes =
  interpretResumable \case
    Codec.Encode cmd ->
      pure (Const (Panes.encode cmd))
    Codec.Decode cmd (Const req) (Const out) ->
      stopEitherWith (CodecError req) (Panes.decode out cmd)
{-# inline interpretCodecPanes #-}
