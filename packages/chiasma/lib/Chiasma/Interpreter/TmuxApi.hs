module Chiasma.Interpreter.TmuxApi where

import Fcf (Eval, Exp, Pure1, type (@@))
import Fcf.Class.Functor (FMap)
import Prelude hiding (send, type (@@))

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.TmuxResponse (TmuxResponse)
import Chiasma.Effect.Codec (Codec, encode, withCodec)
import Chiasma.Effect.TmuxApi (TmuxApi (Schedule, Send), send)
import qualified Chiasma.Effect.TmuxClient as TmuxClient
import Chiasma.Effect.TmuxClient (TmuxClient)

type family (f :: l -> k) <$> (fa :: [l]) :: [k] where
  f <$> fa =
    FMap (Pure1 f) @@ fa

flush ::
  Member (TmuxApi c) r =>
  InterpreterFor (TmuxApi c) r
flush =
  interpret \case
    Send cmd ->
      send cmd
    Schedule cmd ->
      void (send cmd)

interpretTmuxApi ::
  ∀ command i o err r .
  Members [TmuxClient i o, Codec command i o !! err] r =>
  InterpreterFor (TmuxApi command !! err) r
interpretTmuxApi =
  interpretResumable \case
    Send cmd -> do
      restop @_ @(Codec _ _ _) $ withCodec cmd \ encoded -> do
        TmuxClient.send encoded
    Schedule cmd -> do
      encoded <- restop (encode cmd)
      TmuxClient.schedule encoded

data TmuxApiEffect :: Type -> (Type -> Type) -> Exp Effect

type instance Eval (TmuxApiEffect err command) =
  TmuxApi command !! err

type family TmuxApis (commands :: [Type -> Type]) (err :: Type) :: EffectRow where
  TmuxApis commands err =
    FMap (TmuxApiEffect err) @@ commands

class InterpretApis (commands :: [Type -> Type]) err i o r where
  interpretApis :: InterpretersFor (TmuxApis commands err) (TmuxClient i o : r)

instance InterpretApis '[] err i o r where
  interpretApis =
    id

instance (
    r1 ~ (TmuxApis commands err ++ TmuxClient i o : r),
    Member (TmuxClient i o) r1,
    Member (Codec command i o !! err) r1,
    InterpretApis commands err i o r
  ) => InterpretApis (command : commands) err i o r where
    interpretApis =
      interpretApis @commands @err . interpretTmuxApi

type InterpretApisNative commands r =
  InterpretApis commands CodecError TmuxRequest TmuxResponse r

class RestopApis (commands :: [Type -> Type]) err i o r where
  restopApis :: InterpretersFor (TmuxApi <$> commands) (TmuxClient i o : r)

instance RestopApis '[] err i o r where
  restopApis =
    id

instance (
    r1 ~ (TmuxApi <$> commands ++ TmuxClient i o : r),
    Members [TmuxClient i o, Stop err] r1,
    Member (Codec command i o !! err) r1,
    RestopApis commands err i o r
  ) => RestopApis (command : commands) err i o r where
    restopApis =
      restopApis @commands @err @i @o .
      interpretTmuxApi @command @i @o .
      restop @err @(TmuxApi command) .
      raiseUnder

type RestopApisNative commands r =
  RestopApis commands CodecError TmuxRequest TmuxResponse r
