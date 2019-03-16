{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Db where

import App.Lib
import Discord.Types.Prelude (UserId, ChannelId)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Aeson
import Data.Text.IO
import Control.Concurrent.MVar

addUserWallet :: Lock -> UserWallet -> IO ()
addUserWallet lock uw =
  withLock
    (do
      old <- readDb'
      let oldUws = _dbUserWallets old
      if uw `elem` oldUws
        then return ()
        else writeDb' (\db -> db { _dbUserWallets = uw : oldUws })
    )
    lock
  --

readDb :: Lock -> IO AppDb
readDb =
  withLock readDb'

writeDb :: Lock -> (AppDb -> AppDb) -> IO ()
writeDb lock updFn =
  withLock (writeDb' updFn) lock

readDb' :: IO AppDb
readDb' =
  rightOrError "error reading db" $ eitherDecodeFileStrict "./db.json"

writeDb' :: (AppDb -> AppDb) -> IO ()
writeDb' updFn =
  updFn <$> readDb' >>=
    Data.Text.IO.writeFile "./db.json" . prettyJson

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
