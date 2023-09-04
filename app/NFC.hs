module NFC where

import Data.Attoparsec.Text
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)

commentLine :: Parser ()
commentLine = do
  _ <- char '#'
  skipWhile (not . isEndOfLine)
  endOfLine

keyValuePair :: Parser (Text, Text)
keyValuePair = do
  key <- takeTill (== ':')
  _ <- char ':'
  skipSpace
  value <- takeTill ((||) <$> isEndOfLine <*> (== '#'))
  skipWhile (not . isEndOfLine)
  endOfLine

  return (key, value)

keyValueOrComment :: Parser (Maybe (Text, Text))
keyValueOrComment = do
  skipSpace
  choice [Nothing <$ try commentLine, Just <$> try keyValuePair]

parseNfc :: Parser (M.Map Text Text)
parseNfc = do
  pairs <- manyTill keyValueOrComment endOfInput
  return $ M.fromList $ catMaybes pairs
