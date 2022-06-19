module Chiasma.Test.Data.TmuxTestConfig where

import Log (Severity (Info))
import qualified Polysemy.Process.Effect.Pty as Pty

data TmuxTestConfig =
  TmuxTestConfig {
    width :: Pty.Rows,
    height :: Pty.Cols,
    fontSize :: Int,
    gui :: Bool,
    conf :: [Text],
    logLevel :: Severity
  }
  deriving stock (Eq, Show, Generic)

instance Default TmuxTestConfig where
  def =
    TmuxTestConfig 240 61 12 False mempty Info
