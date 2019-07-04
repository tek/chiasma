module Chiasma.Native.StreamParse(
  parseConduit,
  parseBlocks,
  TmuxOutputBlock(..),
) where

import Conduit (ConduitT, mapC, (.|))
import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadThrow)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Text.Parser.Char (CharParsing, anyChar, newline, string)
import Text.Parser.Combinators (choice, many, manyTill, notFollowedBy, skipMany, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)

data End =
  EndSuccess
  |
  EndError
  deriving (Eq, Show)

data TmuxOutputBlock =
  Success [Text]
  |
  Error [Text]
  deriving (Eq, Show)

tillEol :: (Alternative m, CharParsing m) => m Text
tillEol = T.pack <$> manyTill anyChar newline

beginLine :: (Alternative m, CharParsing m, Monad m) => m ()
beginLine = void $ string "%begin" >> tillEol

endLine :: (Alternative m, CharParsing m, Monad m) => m End
endLine = do
  end <- choice [EndSuccess <$ string "%end", EndError <$ string "%error"]
  _ <- tillEol
  return end

notBeginLine :: (Alternative m, CharParsing m, Monad m) => m ()
notBeginLine = void $ notFollowedBy (string "%begin") >> tillEol

-- |Parse a sequence of lines between a %start and a %end line.
-- Tmux pads output lines with a single space on both sides, so strip those if the leading one is present.
parseBlock :: (Alternative m, CharParsing m, Monad m, LookAheadParsing m) => m TmuxOutputBlock
parseBlock = do
  _ <- skipMany notBeginLine
  _ <- beginLine
  dataLines <- manyTill tillEol $ try $ lookAhead endLine
  end <- endLine
  return $ case end of
    EndSuccess -> Success dataLines
    EndError -> Error dataLines

parseBlocks :: (Alternative m, CharParsing m, Monad m, LookAheadParsing m) => m [TmuxOutputBlock]
parseBlocks = do
  result <- many (try parseBlock)
  skipMany tillEol
  return result

parser :: Parser TmuxOutputBlock
parser = parseBlock

parseConduit :: MonadThrow m => ConduitT ByteString TmuxOutputBlock m ()
parseConduit = conduitParser parser .| mapC snd
