module Chiasma.Interpreter.TmuxApi where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Functor (FMap)
import Prelude hiding (send)

import Chiasma.Effect.Codec (Codec, decode, encode)
import Chiasma.Effect.TmuxApi (TmuxApi (Schedule, Send), send)
import qualified Chiasma.Effect.TmuxClient as TmuxClient
import Chiasma.Effect.TmuxClient (TmuxClient)

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
  ∀ command encode decode err r .
  Members [TmuxClient encode decode, Codec command encode decode !! err] r =>
  InterpreterFor (TmuxApi command !! err) r
interpretTmuxApi =
  interpretResumable \case
    Send (cmd :: command a) -> do
      encoded <- restop (encode cmd)
      response <- TmuxClient.send encoded
      restop (decode cmd encoded response)
    Schedule cmd -> do
      encoded <- restop (encode cmd)
      TmuxClient.schedule encoded

data TmuxApiEffect :: Type -> (Type -> Type) -> Exp Effect

type instance Eval (TmuxApiEffect err command) =
  TmuxApi command !! err

type family TmuxApis (commands :: [Type -> Type]) (err :: Type) :: EffectRow where
  TmuxApis commands err =
    FMap (TmuxApiEffect err) @@ commands

class InterpretApis (commands :: [Type -> Type]) err encode decode r where
  interpretApis :: InterpretersFor (TmuxApis commands err) (TmuxClient encode decode : r)

instance InterpretApis '[] err encode decode r where
  interpretApis =
    id

instance (
    r1 ~ (TmuxApis commands err ++ TmuxClient encode decode : r),
    Member (TmuxClient encode decode) r1,
    Member (Codec command encode decode !! err) r1,
    InterpretApis commands err encode decode r
  ) => InterpretApis (command : commands) err encode decode r where
    interpretApis =
      interpretApis @commands @err . interpretTmuxApi
