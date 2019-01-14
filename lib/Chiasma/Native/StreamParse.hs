module Chiasma.Native.StreamParse(
  parseConduit,
  parseBlocks,
) where

import Data.Attoparsec.ByteString (Parser)
import Conduit (ConduitT, (.|), mapC)
import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadThrow)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Functor (void)
import Data.ByteString (ByteString)
import Text.Parser.Char (CharParsing, string, newline, anyChar)
import Text.Parser.Combinators (try, many, manyTill, skipMany, notFollowedBy, choice)

tillEol :: (Alternative m, CharParsing m) => m String
tillEol = manyTill anyChar newline

beginLine :: (Alternative m, CharParsing m, Monad m) => m ()
beginLine = void $ string "%begin" >> tillEol

endLine :: (Alternative m, CharParsing m, Monad m) => m ()
endLine = void $ choice [string "%end", string "%error"] >> tillEol

notBeginLine :: (Alternative m, CharParsing m, Monad m) => m ()
notBeginLine = void $ notFollowedBy (string "%begin") >> tillEol

parseBlock :: (Alternative m, CharParsing m, Monad m) => m [String]
parseBlock = do
  _ <- skipMany notBeginLine
  _ <- beginLine
  manyTill tillEol (try endLine)

parseBlocks :: (Alternative m, CharParsing m, Monad m) => m [[String]]
parseBlocks = do
  result <- many (try parseBlock)
  skipMany tillEol
  return result

parser :: Parser [String]
parser = parseBlock

parseConduit :: MonadThrow m => ConduitT ByteString [String] m ()
parseConduit = conduitParser parser .| mapC snd
