module Chiasma.Native.Parse(
  resultParser,
  resultLines,
) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Text.Parsec (
  ParseError,
  parse,
  many,
  skipMany,
  manyTill,
  try,
  )
import Text.Parsec.Char (endOfLine, string, anyChar)
import Text.Parsec.Text (GenParser)

tillEol :: GenParser st Text
tillEol = T.pack <$> manyTill anyChar endOfLine

beginLine :: GenParser st Text
beginLine = string "%begin" >> tillEol

endLine :: GenParser st Text
endLine = string "%end" >> tillEol

parseBlock :: GenParser st [Text]
parseBlock = do
  _ <- manyTill tillEol (try beginLine)
  manyTill tillEol (try endLine)

resultParser :: GenParser st [[Text]]
resultParser = do
  result <- many (try parseBlock)
  skipMany tillEol
  return result

resultLines :: Text -> Either ParseError [[Text]]
resultLines = parse resultParser "tmux output"
