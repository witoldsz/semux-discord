{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Semux where

import Data.Text
import Data.List
import Data.Time
import Data.Int (Int32, Int64)
import Data.Maybe
import Data.Aeson
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Debug.Trace

getBlock :: String -> Maybe Int32 -> IO (Maybe SemuxBlock)
getBlock semuxApi blockNumber =
  result <$> getSemuxBlock (semuxApi ++ path)
  where
    path = case blockNumber of
      Just n  -> "block-by-number?number=" ++ show (1 + n)
      Nothing -> "latest-block"

-- ApiResponse
data ApiResponse a = ApiResponse
     { success :: Bool
     , message :: !Text
     , result :: Maybe a
     } deriving (Show, Generic)

-- Block
data SemuxBlock = SemuxBlock
  { blockNumber :: Int32
  , blockTxs :: [SemuxTx]
  } deriving (Show)

instance FromJSON (ApiResponse SemuxBlock)

instance FromJSON SemuxBlock where
  parseJSON = withObject "SemuxBlock" $ \o ->
    SemuxBlock <$> fmap read (o .: "number")
               <*> o .: "transactions"

getSemuxBlock :: String -> IO (ApiResponse SemuxBlock)
getSemuxBlock url =
  getResponseBody <$> httpJSON (parseRequest_ url)

-- Transaction
data SemuxTx = SemuxTx
 { txTimestamp :: UTCTime
 , txType :: !Text
 , txFrom :: !Text
 , txTo :: !Text
 , txValue :: !Int64
 } deriving (Show)

instance FromJSON SemuxTx where
  parseJSON = withObject "SemuxTx" $ \o ->
    SemuxTx <$> fmap textToUTC (o .: "timestamp")
            <*> o .: "type"
            <*> o .: "from"
            <*> o .: "to"
            <*> fmap read (o .: "value")

-- UTCTime
textToUTC :: Text -> UTCTime
textToUTC =
  fromJust . parseTimeM False defaultTimeLocale "%s" . unpack . Data.Text.take 10
