{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE RecordWildCards   #-}

module App.App where

import ClassyPrelude
import App.Types
import App.Db
import App.Lib
import App.Semux
import App.Discord
import App.DiscordCmd
import Discord
import Control.Concurrent

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
    (const listenToCmds)

  where
    listenToCmds = do
      nextCmd discord (handleCmd <=< logIncommingCmd)
      listenToCmds

    answer :: Message -> Text -> IO ()
    answer m =
      void . sendMessage discord (messageChannel m)

    handleCmd :: (Message, DiscordCmd) -> IO ()
    handleCmd (m, Hi) =
      answer m
        "Hi! My name is _Semux Online Discord Bot_.\
        \\nWhat do I do, you ask? Well, I am monitoring the Semux blockchain.\
        \ You can tell me what Semux account you want me to watch, and I will tell you when\
        \ there is an incoming transfer.\
        \\n\
        \\nThis is all for a good start, but new features will come, hopefully :) For example:\
        \ what would you say if I could watch over delegates?\
        \\n\
        \\nCommands I understand:\
        \\n- `hi` my greetings and list of commands I can handle.\
        \\n- `add` to let me watch an account for you.\
        \\n- `del` to stop me watching an account.\
        \\n- `list` to see which accounts I do watch for you.\
        \\n\
        \\nGo ahead and type one of these."

    handleCmd (m, Unrecognized t) =
      answer m $
        "`" <> t <> "`?\
        \\nI don't understand. Say `hi` to get us know better :)"

    handleCmd (m, AddHelp) =
      answer m
        "Add a Semux account to my watch list. **I will tell you about each incoming transfer to that address.**\
        \ Use `list` command to see the accounts I watch already.\
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

    handleCmd (m, DelHelp) =
      answer m
        "Removes an account from the list. Use `list` command to see which they are.\
        \\nType:\
        \\n`del addr`\
        \\nwhere:\
        \\n  `addr` is the Semux address (0x…)\
        \\n\
        \\nFor example:\
        \\n`del 0xd45d3b25fd1d72e9da4dab7637814f138437f446`"

    handleCmd (m, AddWallet uw) = do
      ok <- addUserWallet dbLock uw
      answer m $
        withHint $ if ok
          then "Sure, no problem!"
          else "I am watching this account already."
      where
        withHint = (<> " Type `list` to see all.")

    handleCmd (m, DelWallet ref) = do
      ok <- delUserWallet dbLock ref
      answer m $
        withHint $ if ok
          then "OK, let's forget about that account."
          else "There must've been some misunderstading… I haven't been watching this account for you."
      where
        withHint = (<> " Type `list` to see all.")

    handleCmd (m, ListWallets) = do
      uws <- filter matches <$> listWallets dbLock
      answer m $
        case length uws of
          0 -> "I am not watching any accounts for you. Use `add` command."
          1 -> "There is one account I am watching for you:\n" <> prettyUws uws
          x -> "There are " <> tshow x <> " accounts I am watching for you:\n" <> prettyUws uws

      where
        matches uw = _uwChanId uw == messageChannel m
        prettyUw uw = "`" <> _uwAddr uw <> "` _" <> _uwAlias uw <> "_"
        prettyUws uws = intercalate "\n" (prettyUw <$> uws :: [Text])

    querySemuxForNewBlocks = do
      latestBlockNumber <- _dbLatestBlockNumber <$> readSemuxDb dbLock
      maybeBlock <- getBlock semuxApiUrl $ (+1) <$> latestBlockNumber

      mapM_
        (\block -> do
          let blockNr = _blockNumber block
          findings <- matchTxsToWallets (_blockTxs block) <$> listWallets dbLock
          logProcessingBlock blockNr findings
          mapM_ publish findings
          writeSemuxDb dbLock (\newDb -> newDb { _dbLatestBlockNumber = Just blockNr })
        )
        maybeBlock

      threadDelay 10e6
      where
        publish :: (UserWallet, SemuxTx) -> IO ()
        publish (uw,tx) =
          void $ sendMessage discord (_uwChanId uw) (messageFormatter (uw,tx))

        matchTxsToWallets :: [SemuxTx] -> [UserWallet] -> [(UserWallet, SemuxTx)]
        matchTxsToWallets txs uws =
          [(uw, tx) | uw <- uws, tx <- txs
          , _uwAddr uw == _txTo tx
          , _txType tx == "TRANSFER"
          ]

        messageFormatter :: (UserWallet, SemuxTx) -> Text
        messageFormatter (UserWallet{..}, SemuxTx{..}) =
          "```\
          \\nIncoming transfer " <> formatSem _txValue <> " SEM \
          \\nfrom " <> shortAddr _txFrom <> "\
          \\nto " <> _uwAlias <> "\
          \\n```"

-- surveillance

logIncommingCmd :: (Message, DiscordCmd) -> IO (Message, DiscordCmd)
logIncommingCmd arg@(m@Message{..}, cmd) = do
  logText $ author <> ": " <> messageText <> " :: " <> tshow cmd
  return arg
  where
    author = either tshow (pack . userName) messageAuthor

logProcessingBlock :: Int32 -> [a] -> IO ()
logProcessingBlock blockNr matchingTxs =
  logStr $ "Processing block #" <> show blockNr <> finding matchingTxs
  where
    finding [] = ""
    finding xs = " publishing " <> show (length xs) <> " notifications"
