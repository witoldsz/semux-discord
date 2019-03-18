{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Db where

import ClassyPrelude
import App.Lib
import App.Types
import App.DiscordCmd
import Discord.Types.Prelude (UserId, ChannelId)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Aeson
import Control.Concurrent.MVar

listWallets :: Lock -> IO [UserWallet]
listWallets =
  withLock $ readJSON "./db_UserWallet.json"

addUserWallet :: Lock -> UserWallet -> IO Bool
addUserWallet lock uw =
  withLock
    (do
      oldUws <- readJSON "./db_UserWallet.json" :: IO [UserWallet]
      if uw `elem` oldUws
        then return False
        else writeJSON "./db_UserWallet.json" (uw : oldUws) >> return True
    )
    lock

delUserWallet :: Lock -> DelUserWalletRef -> IO Bool
delUserWallet lock DelUserWalletRef{..} =
  withLock
    (do
      oldUws <- readJSON "./db_UserWallet.json" :: IO [UserWallet]
      let newUws = filter notMatches oldUws
      if length oldUws == length newUws
        then return False
        else writeJSON "./db_UserWallet.json" newUws >> return True
    )
    lock
  where
    notMatches UserWallet{..} =
      _uwAddr /= _duwAddr || _uwUserId /= _duwUserId

readSemuxDb :: Lock -> IO SemuxDb
readSemuxDb =
  withLock $ readJSON "./db_SemuxDb.json"

writeSemuxDb :: Lock -> (SemuxDb -> SemuxDb) -> IO ()
writeSemuxDb lock updFn =
  withLock
    (do
      db <- readJSON "./db_SemuxDb.json"
      writeJSON "./db_SemuxDb.json" $ updFn db
    )
    lock

-- Internal:

readJSON :: (FromJSON a) => FilePath -> IO a
readJSON f =
  rightOrError ("error reading " <> f) $ eitherDecodeFileStrict f

writeJSON :: (ToJSON a) => FilePath -> a -> IO ()
writeJSON f =
  writeFileUtf8 f . prettyJson