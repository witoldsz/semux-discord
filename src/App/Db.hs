{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Db where

import Discord.Types.Prelude (UserId)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Aeson

data UserWallet = UserWallet
  { uwAddr :: !Text
  , uwUserId :: !UserId
  , uwLastTx :: !(Maybe Int32)
  } deriving Show

instance FromJSON UserWallet where
  parseJSON = withObject "UserWallet" $ \o ->
    UserWallet <$> o .: "addr"
               <*> o .: "userId"
               <*> o .: "lastTx"

instance ToJSON UserWallet where
  toJSON UserWallet {..} = object
    [ "addr" .= uwAddr
    , "userId" .= uwUserId
    , "lastTx" .= uwLastTx
    ]

data AppDb = AppDb
  { dbLatestBlockNumber :: Maybe Int32
  , dbUserWallets :: [UserWallet]
  } deriving Show

instance FromJSON AppDb where
  parseJSON = withObject "AppDb" $ \o ->
    AppDb <$> o .: "latestBlockNumber"
          <*> o .: "userWallets"

instance ToJSON AppDb where
  toJSON AppDb {..} = object
    [ "latestBlockNumber" .= dbLatestBlockNumber
    , "userWallets" .= dbUserWallets
    ]
