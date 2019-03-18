{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Semux where

import ClassyPrelude
import Text.Read
import Data.Text (takeEnd)
import Data.Time
import Data.Int (Int32, Int64)
import Data.Maybe
import Data.Aeson
import Data.Decimal
import GHC.Generics (Generic)
import Network.HTTP.Simple

getBlock :: String -> Maybe Int32 -> IO (Maybe SemuxBlock)
getBlock semuxApi blockNumber =
  _result <$> getSemuxBlock (semuxApi <> path)
  where
    path = case blockNumber of
      Just n  -> "block-by-number?number=" <> show n
      Nothing -> "latest-block"

-- ApiResponse
data ApiResponse a = ApiResponse
     { _success :: Bool
     , _message :: !Text
     , _result :: Maybe a
     } deriving (Show, Generic)

instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- Block
data SemuxBlock = SemuxBlock
  { _blockNumber :: Int32
  , _blockTxs :: [SemuxTx]
  } deriving (Show)

instance FromJSON SemuxBlock where
  parseJSON = withObject "SemuxBlock" $ \o ->
    SemuxBlock <$> fmap read (o .: "number")
               <*> o .: "transactions"

getSemuxBlock :: String -> IO (ApiResponse SemuxBlock)
getSemuxBlock url =
  getResponseBody <$> httpJSON (parseRequest_ url)

-- Transaction
data SemuxTx = SemuxTx
 { _txTimestamp :: UTCTime
 , _txType :: !Text
 , _txFrom :: !Text
 , _txTo :: !Text
 , _txValue :: !Int64
 , _txHash :: !Text
 } deriving (Show)

instance FromJSON SemuxTx where
  parseJSON = withObject "SemuxTx" $ \o ->
    SemuxTx <$> fmap textToUTC (o .: "timestamp")
            <*> o .: "type"
            <*> o .: "from"
            <*> o .: "to"
            <*> fmap read (o .: "value")
            <*> o .: "hash"

-- UTCTime
textToUTC :: Text -> UTCTime
textToUTC =
  fromJust . parseTimeM False defaultTimeLocale "%s" . unpack . take 10

formatSem :: Int64 -> Text
formatSem =
  pack. show . normalizeDecimal . Decimal 9

shortAddr :: Text -> Text
shortAddr a =
  take 6 a <> "…" <> takeEnd 4 a

isAddr :: Text -> Bool
isAddr a =
  length a == 42 && take 2 a == "0x" && fromMaybe 0 (readMay $ unpack a) > (0 :: Integer)
