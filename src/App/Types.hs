{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Types where

import Data.Aeson
import Discord
import Data.Int (Int32)
import Data.Text (Text)

-- AppDb
data AppDb = AppDb
  { _dbLatestBlockNumber :: Maybe Int32
  , _dbUserWallets :: [UserWallet]
  } deriving Show

instance FromJSON AppDb where
  parseJSON = withObject "AppDb" $ \o ->
    AppDb <$> o .: "latestBlockNumber"
          <*> o .: "userWallets"

instance ToJSON AppDb where
  toJSON AppDb {..} = object
    [ "latestBlockNumber" .= _dbLatestBlockNumber
    , "userWallets" .= _dbUserWallets
    ]

-- UserWallet
data UserWallet = UserWallet
  { _uwAddr :: !Text
  , _uwAlias :: !Text
  , _uwUserId :: !UserId
  , _uwChanId :: !ChannelId
  } deriving (Eq, Show)

instance FromJSON UserWallet where
  parseJSON = withObject "UserWallet" $ \o ->
    UserWallet <$> o .: "addr"
               <*> o .: "alias"
               <*> o .: "userId"
               <*> o .: "chanId"

instance ToJSON UserWallet where
  toJSON UserWallet {..} = object
    [ "addr"   .= _uwAddr
    , "alias"  .= _uwAlias
    , "userId" .= _uwUserId
    , "chanId" .= _uwChanId
    ]
