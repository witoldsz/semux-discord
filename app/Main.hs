{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Prelude hiding (mapM_)
import System.Environment
import Data.Text
import App.Db
import App.Lib
import App.Semux
import App.Discord
import Control.Concurrent
import Data.Foldable (mapM_)

main :: IO ()
main = do
  semuxApi <- getEnv "SEMUX_API"
  token <- getEnv "DISCORD_SECRET"

  logEmptyLine

  startDiscord token (useDiscord semuxApi)

  return ()

useDiscord :: String -> Discord -> IO ()
useDiscord semuxApi discord = do

  db <- readDb
  let latestBlockNumber = _dbLatestBlockNumber db
  maybeBlock <- getBlock semuxApi $ (+1) <$> latestBlockNumber

  mapM_
    (\block -> do
      let blockNr = _blockNumber block
      logStr $ "Processing block #" ++ show blockNr
      mapM_
        (sendMessage discord messageFormatter)
        (matchTxsToWallets (_dbUserWallets db) (_blockTxs block))

      let newDb = db { _dbLatestBlockNumber = Just blockNr }
      writeDb newDb
    )
    maybeBlock

  logText "Will sleep 10 secs…"
  threadDelay 10e6

  useDiscord semuxApi discord

matchTxsToWallets :: [UserWallet] -> [SemuxTx] -> [(UserWallet, SemuxTx)]
matchTxsToWallets uws txs =
  [(uw, tx) | uw <- uws, tx <- txs, _uwAddr uw == _txTo tx, _txType tx == "TRANSFER"]

messageFormatter :: (UserWallet, SemuxTx) -> Text
messageFormatter (UserWallet{..}, SemuxTx{..}) =
  mconcat
    [ "```"
    , "\nIncoming transfer"
    , "\n📥 ", shortAddr _txTo
    , "\n💰 ", formatSem _txValue, " SEM "
    , "\n📤 ", shortAddr _txFrom
    , "\n```"
    ]