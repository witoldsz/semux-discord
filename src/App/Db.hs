{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Db where

import App.Lib
import App.Types
import App.DiscordCmd
import Discord.Types.Prelude (UserId, ChannelId)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Aeson
import Data.Text.IO
import Control.Concurrent.MVar

addUserWallet :: Lock -> UserWallet -> IO Bool
addUserWallet lock uw =
  withLock
    (do
      oldUws <- _dbUserWallets <$> readDb'
      if uw `elem` oldUws
        then return False
        else writeDb' (\db -> db { _dbUserWallets = uw : oldUws }) >> return True
    )
    lock
  --

delUserWallet :: Lock -> DelUserWalletRef -> IO Bool
delUserWallet lock DelUserWalletRef{..} =
  withLock
    (do
      oldUws <- _dbUserWallets <$> readDb'
      let newUws = filter (not . matches) oldUws
      if length oldUws == length newUws
        then return False
        else writeDb' (\db -> db { _dbUserWallets = newUws }) >> return True
    )
    lock
  where
    matches UserWallet{..} =
      _uwAddr == _duwAddr && _uwUserId == _duwUserId

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

