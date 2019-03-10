{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE RecordWildCards   #-}

module App.App where

import Prelude hiding (mapM_)
import Data.Text
import App.Db
import App.Lib
import App.Semux
import App.Discord
import Control.Concurrent
import Data.Foldable (mapM_)

data Configuration = Configuration
  { _semuxApiUrl :: String
  , _dicordSecret :: String
  }

app :: Configuration -> IO ()
app Configuration{..} =
  startDiscord _dicordSecret useDiscord
  where
    useDiscord :: Discord -> IO ()
    useDiscord discord = do

      db <- readDb
      let latestBlockNumber = _dbLatestBlockNumber db
      maybeBlock <- getBlock _semuxApiUrl $ (+1) <$> latestBlockNumber

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

      useDiscord discord

matchTxsToWallets :: [UserWallet] -> [SemuxTx] -> [(UserWallet, SemuxTx)]
matchTxsToWallets uws txs =
  [(uw, tx) | uw <- uws, tx <- txs
    , _uwAddr uw == _txTo tx
    , _txType tx == "TRANSFER"
    ]

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