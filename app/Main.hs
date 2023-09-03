{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString, putStr, readFile, writeFile)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Read
import Options.Applicative
import Text.Printf
import Prelude hiding (putStr, readFile, writeFile)

data Args = Args
  { file :: String
  , outFile :: Maybe String
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

  input <- decodeUtf8 <$> readFile (file options)

  let nfcData =
        M.map (T.dropWhile ((||) <$> (== ':') <*> (== ' ')))
          . M.fromList
          . map (T.breakOn ":")
          . filter (not . T.null)
          . map (T.strip . fst . T.breakOn "#")
          $ T.lines input

      fields =
        [ nfcData M.! "Mifare version"
        , "00000086"
        , nfcData M.! "Signature"
        , T.pack $ printf "%06X" (parseInt $ nfcData M.! "Counter 0")
        , T.pack $ printf "%02X" (parseInt $ nfcData M.! "Tearing 0")
        , T.pack $ printf "%06X" (parseInt $ nfcData M.! "Counter 1")
        , T.pack $ printf "%02X" (parseInt $ nfcData M.! "Tearing 1")
        , T.pack $ printf "%06X" (parseInt $ nfcData M.! "Counter 2")
        , T.pack $ printf "%02X" (parseInt $ nfcData M.! "Tearing 2")
        ]
          <> map ((nfcData M.!) . ("Page " <>) . T.pack . show) [0 .. 134]

      b16Bytes = encodeUtf8 . T.filter (not . isSpace) . T.concat $ fields

      bytes :: ByteString
      bytes = case convertFromBase Base16 b16Bytes of
        Right b -> b
        Left err -> error err

  case outFile options of
    Nothing -> putStr bytes
    Just p -> writeFile p bytes
