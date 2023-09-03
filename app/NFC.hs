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
  skipSpace
  key <- takeTill ((||) <$> (== ':') <*> (== '#'))
  _ <- char ':'
  skipSpace
  value <- takeTill ((||) <$> isEndOfLine <*> (== '#'))
  endOfLine

  return (key, value)

keyValueOrComment :: Parser (Maybe (Text, Text))
keyValueOrComment = do
  skipSpace
  choice [Just <$> try keyValuePair, Nothing <$ try commentLine]

parseNfc :: Parser (M.Map Text Text)
parseNfc = do
  pairs <- manyTill keyValueOrComment endOfInput
  return $ M.fromList $ catMaybes pairs
