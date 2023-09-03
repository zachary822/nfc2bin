{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Read
import NFC
import Options.Applicative
import Text.Printf

data Args = Args
  { file :: String
  , outFile :: Maybe FilePath
  }
  deriving (Eq, Show)

args :: Parser Args
args = Args <$> argument str (metavar "FILE") <*> optional (argument str (metavar "OUTPUT"))

opts :: ParserInfo Args
opts = info (args <**> helper) (fullDesc <> progDesc "convert nfc to eml")

parseInt :: Text -> Integer
parseInt = fst . fromRight (0, "") . decimal

main :: IO ()
main = do
  options <- execParser opts

  input <- decodeUtf8 <$> B.readFile (file options)

  let nfcData = fromRight mempty (parseOnly parseNfc input)
      fields =
        [ nfcData M.! "Mifare version"
        , "00000086"
        , nfcData M.! "Signature"
        , T.pack $ printf "%06X" (parseInt $ nfcData M.! "Counter 0")
        , nfcData M.! "Tearing 0"
        , T.pack $ printf "%06X" (parseInt $ nfcData M.! "Counter 1")
        , nfcData M.! "Tearing 1"
        , T.pack $ printf "%06X" (parseInt $ nfcData M.! "Counter 2")
        , nfcData M.! "Tearing 2"
        ]
          <> map ((nfcData M.!) . ("Page " <>) . T.pack . show) ([0 .. 134] :: [Integer])

      b16Bytes = encodeUtf8 . T.filter (not . isSpace) . T.concat $ fields

  case convertFromBase Base16 b16Bytes of
    Right bytes ->
      case outFile options of
        Nothing -> B.putStr bytes
        Just "-" -> B.putStr bytes
        Just p -> B.writeFile p bytes
    Left err -> fail err
