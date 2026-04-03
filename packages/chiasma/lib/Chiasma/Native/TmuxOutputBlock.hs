module Chiasma.Native.TmuxOutputBlock where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString.Internal (packChars)
import Prelude hiding (noneOf, try)
import Text.Parser.Char (CharParsing, anyChar, char, newline, noneOf, string)
import Text.Parser.Combinators (choice, manyTill, notFollowedBy, skipMany, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)

import qualified Chiasma.Data.TmuxEvent as TmuxEvent
import Chiasma.Data.TmuxEvent (TmuxEvent)
import Chiasma.Data.TmuxNotification (TmuxNotification (..))
import Chiasma.Data.TmuxOutputBlock (End (EndError, EndSuccess), TmuxOutputBlock (Error, Success))

tillEol :: (CharParsing m) => m Text
tillEol = decodeUtf8 . packChars <$> manyTill anyChar newline

beginLine :: (CharParsing m, Monad m) => m ()
beginLine = void $ string "%begin" >> tillEol

endLine :: (CharParsing m) => m End
endLine = do
  end <- choice [EndSuccess <$ string "%end", EndError <$ string "%error"]
  _ <- tillEol
  pure end

notBeginLine :: (CharParsing m, Monad m) => m ()
notBeginLine = void $ notFollowedBy (string "%begin") >> tillEol

word :: CharParsing m => m Text
word = toText <$> some (noneOf " \n")

notificationLine :: (CharParsing m, Monad m) => m TmuxNotification
notificationLine = do
  notFollowedBy (string "%begin")
  _ <- char '%'
  name <- word
  args <- many (char ' ' *> word)
  _ <- newline
  pure TmuxNotification {name, args}

-- | Parse a sequence of lines between a %start and a %end line.
-- Tmux pads output lines with a single space on both sides, so strip those if the leading one is present.
parseBlock :: (CharParsing m, Monad m, LookAheadParsing m) => m TmuxOutputBlock
parseBlock = do
  _ <- skipMany notBeginLine
  _ <- beginLine
  dataLines <- manyTill tillEol $ try (lookAhead endLine)
  endLine <&> \case
    EndSuccess -> Success dataLines
    EndError -> Error dataLines

parseBlocks :: (CharParsing m, Monad m, LookAheadParsing m) => m [TmuxOutputBlock]
parseBlocks = do
  result <- many (try parseBlock)
  skipMany tillEol
  pure result

-- | Parse either a notification line or a command response block.
parseMessage :: (CharParsing m, Monad m, LookAheadParsing m) => m TmuxEvent
parseMessage =
  choice
    [ TmuxEvent.Notification <$> try notificationLine
    , TmuxEvent.Response <$> parseBlock
    ]

parser :: Parser TmuxOutputBlock
parser = parseBlock

messageParser :: Parser TmuxEvent
messageParser = parseMessage
