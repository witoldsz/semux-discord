{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Semux (getLastCoinbase) where

import Data.Text
import Data.List
import Data.Time
import Data.Maybe
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

pageSize :: Int
pageSize = 100

getLastCoinbase :: String -> String -> IO UTCTime
getLastCoinbase semuxApi delegate = do
  account <- getRight $ getAccount semuxApi delegate
  let txCount = (transactionCount . result) account
  tx <- findLastCoinbase txCount
  print tx
  return $ timestamp tx

  where
  findLastCoinbase :: Int -> IO Transaction
  findLastCoinbase lastTx = do
    response <- getRight $ getTransactions semuxApi delegate txRange
    let txsRev = (Data.List.reverse . result) response
    let lastCoinbase = Data.List.find (\i -> transactionType i == "COINBASE") txsRev
    solution lastCoinbase
    where
      from = max 0 (lastTx - pageSize)
      txRange = (from, lastTx)

      solution :: Maybe Transaction -> IO Transaction
      solution (Just coinbase ) =
        return coinbase
      solution Nothing =
        if (from > 0)
          then findLastCoinbase from
          else fail "No COINBASE found"

getRight :: IO (Either String a) -> IO a
getRight x =
  x >>= either fail return

-- Account
data Account = Account
  { transactionCount :: Int
  } deriving (Show, Generic)

instance FromJSON Account

getAccount :: String -> String -> IO (Either String (Response Account))
getAccount semuxApi addr =
  eitherDecode <$> fetchJson
  where
    fetchJson :: IO B.ByteString
    fetchJson =
      putStrLn url >> simpleHttp url
      where
        url = semuxApi ++ "account?address=" ++ addr

-- Transaction
data Transaction = Transaction
 { timestamp :: UTCTime
 , transactionType :: !Text
 } deriving (Show)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \v -> Transaction
    <$> fmap textToUTC (v .: "timestamp")
    <*> v .: "type"

getTransactions :: String -> String -> (Int, Int) -> IO (Either String (Response [Transaction]))
getTransactions semuxApi addr (from, to) =
  eitherDecode <$> fetchJson
  where
    fetchJson :: IO B.ByteString
    fetchJson =
      putStrLn url >> simpleHttp url
      where
        url =
          semuxApi ++ "account/transactions?address=" ++ addr ++ "&from=" ++ (show from) ++ "&to=" ++ (show to)

-- Response
data Response a = Response
  { success :: Bool
  , message :: !Text
  , result :: a
  } deriving (Show, Generic)

instance FromJSON (Response [Transaction])
instance FromJSON (Response Account)

-- UTCTime
textToUTC :: Text -> UTCTime
textToUTC = fromJust . (parseTimeM False defaultTimeLocale "%s") . unpack . (Data.Text.take 10)
