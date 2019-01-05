module Chiasma.Native.Parse(
  resultParser,
  resultLines,
) where

import Text.ParserCombinators.Parsec (
  GenParser,
  ParseError,
  parse,
  many,
  skipMany,
  manyTill,
  notFollowedBy,
  try,
  )
import Text.Parsec.Char (endOfLine, string, anyChar)

tillEol :: GenParser Char st String
tillEol = manyTill anyChar endOfLine

beginLine :: GenParser Char st String
beginLine = string "%begin" >> tillEol

endLine :: GenParser Char st String
endLine = string "%end" >> tillEol

notBeginLine :: GenParser Char st String
notBeginLine = notFollowedBy (string "%begin") >> tillEol

parseBlock :: GenParser Char st [String]
parseBlock = do
  _ <- skipMany notBeginLine
  _ <- beginLine
  manyTill tillEol (try endLine)

resultParser :: GenParser Char st [[String]]
resultParser = do
  result <- many (try parseBlock)
  skipMany tillEol
  return result

resultLines :: String -> Either ParseError [[String]]
resultLines = parse resultParser "tmux output"
