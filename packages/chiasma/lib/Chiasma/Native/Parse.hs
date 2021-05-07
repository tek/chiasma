module Chiasma.Native.Parse where

import qualified Data.Text as T (pack)
import Prelude hiding (many)
import Text.Parsec (
  ParseError,
  many,
  manyTill,
  parse,
  skipMany,
  try,
  )
import Text.Parsec.Char (anyChar, endOfLine, string)
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
resultParser =
  many (try parseBlock) <* skipMany tillEol

resultLines :: Text -> Either ParseError [[Text]]
resultLines = parse resultParser "tmux output"
