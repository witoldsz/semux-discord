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
import App.DiscordCmd
import Discord
import Control.Concurrent
import Control.Monad
import Control.Exception.Base
import Data.Foldable (mapM_)

data Configuration = Configuration
  { semuxApiUrl :: String
  , dicordSecret :: String
  }

app :: Configuration -> IO ()
app Configuration{..} = do
  dbLock <- newLock
  startDiscord dicordSecret (useDiscord dbLock semuxApiUrl)

useDiscord :: Lock -> String -> Discord -> IO ()
useDiscord dbLock semuxApiUrl discord =

  bracket
    (forkIO $ forever querySemuxForNewBlocks)
    killThread
    (\_ -> listenToCmds)

  where
    listenToCmds =
      nextCmd discord handleCmd >> listenToCmds

    handleCmd :: Message -> DiscordCmd -> IO ()
    handleCmd msg Hi = do
      logStr "New message arrived"
      db <- readDb dbLock
      let uws = Prelude.filter (\uw -> _uwChanId uw == messageChannel msg)
                               (_dbUserWallets db)
      sendMessage discord (messageChannel msg) (pack $ show uws)
      return ()

    handleCmd m (AddWallet uw) = do
      addUserWallet dbLock uw
      sendMessage discord (_uwChanId uw) "Sure, no problem…"
      return ()
      where
        msg UserWallet{..} =
          "Sure, no problem! Account `" <> _uwAddr <> "` added to watch list as _" <> _uwAlias <> "_."

    handleCmd m AddHelp = do
      sendMessage discord (messageChannel m)
        "Add a Semux account to my watch list. **I will tell you about each incoming transfer to that address.**\
        \\nType:\
        \\n`add addr` or\
        \\n`add addr alias`,\
        \\nwhere:\
        \\n  `addr` is the Semux address (0x…)\
        \\n  `alias` optional name I will use instead of address\
        \\n\
        \\nFor example:\
        \\n`add 0xd45d3b25fd1d72e9da4dab7637814f138437f446`\
        \\nor\
        \\n`add 0xd45d3b25fd1d72e9da4dab7637814f138437f446 Account One`"
      return ()

    handleCmd _ (Unrecognized e) = logStr (show e)

    querySemuxForNewBlocks = do
      db <- readDb dbLock
      let latestBlockNumber = _dbLatestBlockNumber db
      maybeBlock <- getBlock semuxApiUrl $ (+1) <$> latestBlockNumber

      mapM_
        (\block -> do
          let blockNr = _blockNumber block
          logStr $ "Processing block #" ++ show blockNr
          mapM_
            publish
            (matchTxsToWallets (_dbUserWallets db) (_blockTxs block))

          writeDb dbLock (\newDb -> newDb { _dbLatestBlockNumber = Just blockNr })
        )
        maybeBlock

      logText "Will sleep 10 secs…"
      threadDelay 10e6
      where
        publish :: (UserWallet, SemuxTx) -> IO (Either RestCallException Message)
        publish (uw,tx) = sendMessage discord (_uwChanId uw) (messageFormatter (uw,tx))

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
