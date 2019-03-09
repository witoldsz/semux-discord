{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Discord where

import App.Semux
import App.Db
import App.Lib
import Discord
import Data.Text
import Control.Exception.Base

type Discord = (RestChan, Gateway, [ThreadIdType])

startDiscord :: String -> (Discord -> IO ()) -> IO ()
startDiscord token =
  bracket
    (loginRestGateway (Auth (pack token)))
    stopDiscord

sendMessage :: Discord
            -> ((UserWallet, SemuxTx) -> Text)
            -> (UserWallet, SemuxTx)
            -> IO (Either RestCallException Message)
sendMessage dis formatter (uw, tx) = do
  logText "sending message…"
  restCall dis $ CreateMessage (_uwChanId uw) (formatter (uw, tx))