{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals       #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (log)
import System.Environment
import Data.Aeson
import App.Db
import App.Lib (rightOrError, log, logEmptyLine, logJ, logText, logStr)
import App.Semux
import Discord
import Control.Exception.Base
import Control.Concurrent
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  semuxApi <- getEnv "SEMUX_API"
  token <- getEnv "DISCORD_SECRET"
  chanid <- getEnv "DISCORD_CHANNEL_ID"

  logEmptyLine

  db <- rightOrError "error reading db" $ eitherDecodeFileStrict "./db.json" :: IO AppDb
  logJ db

  bracket (loginRestGateway (Auth token))
          stopDiscord
          (useDiscord db)

  return ()

  where
    useDiscord :: AppDb -> Discord -> IO ()
    useDiscord db dis = do
      let latestBlockNumber = dbLatestBlockNumber db
      maybeBlock <- getBlock semuxApi latestBlockNumber
      nextBlockNumber <- case maybeBlock of
        Just block -> do
          let nextBlockNumber = blockNumber block
          let userWallets = dbUserWallets db
          let msg = T.pack $ "`" ++ show db ++ "`\n`" ++ show block ++ "`"
          -- restCall dis $ CreateMessage chanid msg
          logText msg
          return $ Just nextBlockNumber

        Nothing ->
          return latestBlockNumber

      logStr $ "nextBlockNumber = " ++ show nextBlockNumber
      logText "Will sleep 10 secs…"
      threadDelay 10e6
      useDiscord (db { dbLatestBlockNumber = nextBlockNumber }) dis

type Discord = (RestChan, Gateway, [ThreadIdType])
