{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Types where

import Data.Aeson
import Discord
import Data.Int (Int32)
import Data.Text (Text)

-- SemuxDb
data SemuxDb = SemuxDb
  { _dbLatestBlockNumber :: Maybe Int32
  } deriving Show

instance FromJSON SemuxDb where
  parseJSON = withObject "SemuxDb" $ \o ->
    SemuxDb <$> o .: "latestBlockNumber"

instance ToJSON SemuxDb where
  toJSON SemuxDb {..} = object
    [ "latestBlockNumber" .= _dbLatestBlockNumber
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
