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
import Network.HTTP.Simple

pageSize :: Int
pageSize = 100

getLastCoinbase :: String -> String -> IO UTCTime
getLastCoinbase semuxApi delegate = do
  account <- getAccount semuxApi delegate
  let txCount = (transactionCount . result) account
  tx <- findLastCoinbase txCount
  print tx
  return $ timestamp tx

  where
    findLastCoinbase :: Int -> IO Transaction
    findLastCoinbase lastTx = do
      response <- getTransactions semuxApi delegate txRange
      let txsRev = (Data.List.reverse . result) response
      let lastCoinbase = Data.List.find (\i -> transactionType i == "COINBASE") txsRev
      case lastCoinbase of
        Just coinbase -> return coinbase
        Nothing
          | from > 0  -> findLastCoinbase from
          | otherwise -> fail "No COINBASE found"

      where
        from = max 0 (lastTx - pageSize)
        txRange = (from, lastTx)

-- Account
data Account = Account
  { transactionCount :: Int
  } deriving (Show, Generic)

instance FromJSON Account

getAccount :: String -> String -> IO (ApiResponse Account)
getAccount semuxApi addr =
  getResponseBody <$> httpJSON (parseRequest_ url)
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

getTransactions :: String -> String -> (Int, Int) -> IO (ApiResponse [Transaction])
getTransactions semuxApi addr (from, to) =
  getResponseBody <$> httpJSON (parseRequest_ url)
  where
    url =
      semuxApi ++ "account/transactions?address=" ++ addr ++ "&from=" ++ show from ++ "&to=" ++ show to

-- Response
data ApiResponse a = Response
     { success :: Bool
     , message :: !Text
     , result :: a
     } deriving (Show, Generic)

instance FromJSON (ApiResponse [Transaction])
instance FromJSON (ApiResponse Account)

-- UTCTime
textToUTC :: Text -> UTCTime
textToUTC = fromJust . parseTimeM False defaultTimeLocale "%s" . unpack . Data.Text.take 10
