{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where

import Data.Text (Text, pack)
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

type Discord = (RestChan, Gateway, [ThreadIdType])

main :: IO ()
main = do
  semuxApi <- getEnv "SEMUX_API"
  token <- pack <$> getEnv "DISCORD_SECRET"
  -- chanid <- getEnv "DISCORD_CHANNEL_ID"

  logEmptyLine

  db <- rightOrError "error reading db" $ eitherDecodeFileStrict "./db.json" :: IO AppDb
  logJ db

  bracket (loginRestGateway (Auth token))
          stopDiscord
          (useDiscord semuxApi db)

  return ()

  where
    useDiscord :: String -> AppDb -> Discord -> IO ()
    useDiscord semuxApi db dis = do
      let latestBlockNumber = dbLatestBlockNumber db
      maybeBlock <- getBlock semuxApi latestBlockNumber
      nextBlockNumber <- case maybeBlock of
        Just block -> do
          let nextBlockNumber = blockNumber block

          sequence_ $
            sendMessage dis messageFormatter
              <$> matchTxsToWallets (dbUserWallets db) (blockTxs block)

          -- restCall dis $ CreateMessage 551523909074288677 undefined
          -- logText msg
          return $ Just nextBlockNumber

        Nothing ->
          return latestBlockNumber

      logStr $ "nextBlockNumber = " ++ show nextBlockNumber
      logText "Will sleep 10 secs…"
      threadDelay 10e6

      let newDb = db { dbLatestBlockNumber = nextBlockNumber }
      useDiscord semuxApi newDb dis

matchTxsToWallets :: [UserWallet] -> [SemuxTx] -> [(UserWallet, SemuxTx)]
matchTxsToWallets uws txs =
    [(uw, tx) | uw <- uws, tx <- txs, uwAddr uw == txTo tx, txType tx == "TRANSFER"]

messageFormatter :: (UserWallet, SemuxTx) -> Text
messageFormatter (UserWallet{..}, SemuxTx{..}) =
  mconcat
    [ "Incoming transfer: https://semux.info/explorer/transaction/", txHash
    , "\n```"
    , "\n📥 ", shortAddr txTo
    , "\n💰 ", formatSem txValue, " SEM "
    , "\n📤 ", shortAddr txFrom
    , "\n```"
    ]

sendMessage :: Discord
            -> ((UserWallet, SemuxTx) -> Text)
            -> (UserWallet, SemuxTx)
            -> IO (Either RestCallException Message)
sendMessage dis formatter (uw, tx) = do
  logText "sending message…"
  restCall dis $ CreateMessage (uwChanId uw) (formatter (uw, tx))