module Chiasma.Command.Window(
  windows,
  window,
  doesWindowExist,
  newWindow,
  sessionWindows,
  splitWindow,
  newSessionWindow,
) where

import Chiasma.Codec.Data (Pane, Window(Window))
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Data.TmuxId (SessionId, TmuxId(formatId), WindowId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Monad.Tmux as Tmux (read, unsafeReadFirst, unsafeReadOne)
import Control.Monad.Free.Class (MonadFree)
import Data.Foldable (find)

sameId :: WindowId -> Window -> Bool
sameId target (Window i _ _) = target == i

windows :: MonadFree TmuxThunk m => m [Window]
windows =
  Tmux.read "list-windows" ["-a"]

window :: MonadFree TmuxThunk m => WindowId -> m (Maybe Window)
window windowId =
  find (sameId windowId) <$> windows

sessionWindows :: MonadFree TmuxThunk m => SessionId -> m [Window]
sessionWindows sid =
  Tmux.read "list-windows" ["-t", formatId sid]

newSessionWindow :: MonadFree TmuxThunk m => SessionId -> m Window
newSessionWindow sid =
  Tmux.unsafeReadOne "list-windows" ["-t", formatId sid]

doesWindowExist :: MonadFree TmuxThunk m => WindowId -> m Bool
doesWindowExist windowId =
  any (sameId windowId) <$> windows

newWindow :: MonadFree TmuxThunk m => SessionId -> Ident -> m Window
newWindow sid name =
  Tmux.unsafeReadOne "new-window" ["-t", formatId sid, "-n", identString name, "-P"]

splitWindow ::
  (MonadFree TmuxThunk m) =>
  FilePath ->
  WindowId ->
  m Pane
splitWindow dir windowId =
  Tmux.unsafeReadFirst "split-window" ["-t", formatId windowId, "-d", "-P", "-c", dir]
