{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Db where

import App.Lib
import Discord.Types.Prelude (UserId, ChannelId)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Aeson
import Data.Text.IO

readDb :: IO AppDb
readDb =
  rightOrError "error reading db" $ eitherDecodeFileStrict "./db.json"

writeDb :: AppDb -> IO ()
writeDb =
  Data.Text.IO.writeFile "./db.json" . prettyJson

data UserWallet = UserWallet
  { _uwAddr :: !Text
  , _uwUserId :: !UserId
  , _uwChanId :: !ChannelId
  , _uwLastTx :: !(Maybe Int32)
  } deriving Show

instance FromJSON UserWallet where
  parseJSON = withObject "UserWallet" $ \o ->
    UserWallet <$> o .: "addr"
               <*> o .: "userId"
               <*> o .: "chanId"
               <*> o .: "lastTx"

instance ToJSON UserWallet where
  toJSON UserWallet {..} = object
    [ "addr" .= _uwAddr
    , "userId" .= _uwUserId
    , "chanId" .= _uwChanId
    , "lastTx" .= _uwLastTx
    ]

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
