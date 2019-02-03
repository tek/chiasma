module Chiasma.Native.StreamParse(
  parseConduit,
  parseBlocks,
  TmuxOutputBlock(..),
) where

import Conduit (ConduitT, (.|), mapC)
import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadThrow)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Functor (void)
import Text.Parser.Char (CharParsing, string, newline, anyChar)
import Text.Parser.Combinators (try, many, manyTill, skipMany, notFollowedBy, choice)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)

data End =
  EndSuccess
  |
  EndError
  deriving (Eq, Show)

data TmuxOutputBlock =
  Success [String]
  |
  Error [String]
  deriving (Eq, Show)

tillEol :: (Alternative m, CharParsing m) => m String
tillEol = manyTill anyChar newline

beginLine :: (Alternative m, CharParsing m, Monad m) => m ()
beginLine = void $ string "%begin" >> tillEol

endLine :: (Alternative m, CharParsing m, Monad m) => m End
endLine = do
  end <- choice [EndSuccess <$ string "%end", EndError <$ string "%error"]
  tillEol
  return end

notBeginLine :: (Alternative m, CharParsing m, Monad m) => m ()
notBeginLine = void $ notFollowedBy (string "%begin") >> tillEol

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
