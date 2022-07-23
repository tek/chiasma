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
  (∀ x . cmd x -> TmuxRequest) ->
  (∀ x . TmuxResponse -> cmd x -> Either DecodeError x) ->
  InterpreterFor (Codec cmd TmuxRequest TmuxResponse !! CodecError) r
interpretCodecNative enc dec =
  interpretResumableH \case
    Codec.Encode cmd ->
      pureT (enc cmd)
    Codec.WithCodec cmd use -> do
      let req = enc cmd
      out <- runTSimple (use req)
      Inspector ins <- getInspectorT
      pureT =<< stopEitherWith (CodecError req) (dec (fold (ins out)) cmd)

interpretCodecTmuxCommand ::
  InterpreterFor (Codec TmuxCommand TmuxRequest TmuxResponse !! CodecError) r
interpretCodecTmuxCommand =
  interpretCodecNative TmuxCommand.encode TmuxCommand.decode
{-# inline interpretCodecTmuxCommand #-}

interpretCodecPanes ::
  TmuxCodec p =>
  InterpreterFor (Codec (Panes p) TmuxRequest TmuxResponse !! CodecError) r
interpretCodecPanes =
  interpretCodecNative Panes.encode Panes.decode
{-# inline interpretCodecPanes #-}

interpretCodecPure ::
  (∀ a . command a -> Sem r (Either Text a)) ->
  InterpreterFor (Codec command () decode !! Text) r
interpretCodecPure run =
  interpretResumableH \case
    Codec.Encode _ ->
      pureT ()
    Codec.WithCodec cmd _ ->
      either stop pureT =<< raise (raise (run cmd))
