module Chiasma.Interpreter.Codec where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Data.CodecError (CodecError (CodecError))
import Chiasma.Data.DecodeError (DecodeError)
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (Panes)
import qualified Chiasma.Data.TmuxCommand as TmuxCommand
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.TmuxResponse (TmuxResponse)
import qualified Chiasma.Effect.Codec as Codec
import Chiasma.Effect.Codec (Codec)

interpretCodecNative ::
  Member RunStop r =>
  (∀ x . cmd x -> TmuxRequest) ->
  (∀ x . TmuxResponse -> cmd x -> Either DecodeError x) ->
  InterpreterFor (Codec cmd TmuxRequest TmuxResponse !! CodecError) r
interpretCodecNative enc dec =
  interpretResumableH \case
    Codec.Encode cmd ->
      pure (enc cmd)
    Codec.WithCodec cmd use -> do
      let req = enc cmd
      out <- runH (use req)
      stopEitherWith (CodecError req) (dec out cmd)

interpretCodecTmuxCommand ::
  Member RunStop r =>
  InterpreterFor (Codec TmuxCommand TmuxRequest TmuxResponse !! CodecError) r
interpretCodecTmuxCommand =
  interpretCodecNative TmuxCommand.encode TmuxCommand.decode
{-# inline interpretCodecTmuxCommand #-}

interpretCodecPanes ::
  Member RunStop r =>
  TmuxCodec p =>
  InterpreterFor (Codec (Panes p) TmuxRequest TmuxResponse !! CodecError) r
interpretCodecPanes =
  interpretCodecNative Panes.encode Panes.decode
{-# inline interpretCodecPanes #-}

interpretCodecPure ::
  Member RunStop r =>
  (∀ a . command a -> Sem r (Either Text a)) ->
  InterpreterFor (Codec command () decode !! Text) r
interpretCodecPure run =
  interpretResumableH \case
    Codec.Encode _ ->
      unit
    Codec.WithCodec cmd _ ->
      stopEither =<< insertAt @0 (run cmd)
