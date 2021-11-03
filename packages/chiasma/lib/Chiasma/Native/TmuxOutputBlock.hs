module Chiasma.Native.TmuxOutputBlock where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString.Internal (packChars)
import Prelude hiding (try)
import Text.Parser.Char (CharParsing, anyChar, newline, string)
import Text.Parser.Combinators (choice, manyTill, notFollowedBy, skipMany, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)

import Chiasma.Data.TmuxOutputBlock (End (EndError, EndSuccess), TmuxOutputBlock (Error, Success))

tillEol :: (Alternative m, CharParsing m) => m Text
tillEol = decodeUtf8 . packChars <$> manyTill anyChar newline

beginLine :: (Alternative m, CharParsing m, Monad m) => m ()
beginLine = void $ string "%begin" >> tillEol

endLine :: (Alternative m, CharParsing m) => m End
endLine = do
  end <- choice [EndSuccess <$ string "%end", EndError <$ string "%error"]
  _ <- tillEol
  pure end

notBeginLine :: (Alternative m, CharParsing m, Monad m) => m ()
notBeginLine = void $ notFollowedBy (string "%begin") >> tillEol

-- |Parse a sequence of lines between a %start and a %end line.
-- Tmux pads output lines with a single space on both sides, so strip those if the leading one is present.
parseBlock :: (Alternative m, CharParsing m, Monad m, LookAheadParsing m) => m TmuxOutputBlock
parseBlock = do
  _ <- skipMany notBeginLine
  _ <- beginLine
  dataLines <- manyTill tillEol $ try (lookAhead endLine)
  end <- endLine
  pure $ case end of
    EndSuccess -> Success dataLines
    EndError -> Error dataLines

parseBlocks :: (Alternative m, CharParsing m, Monad m, LookAheadParsing m) => m [TmuxOutputBlock]
parseBlocks = do
  result <- many (try parseBlock)
  skipMany tillEol
  pure result

parser :: Parser TmuxOutputBlock
parser = parseBlock
