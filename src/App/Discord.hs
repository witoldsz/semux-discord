{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App.Discord where

import App.Semux
import App.Db
import App.Lib
import App.DiscordCmd
import Discord
import Data.Text
import Data.Maybe
import Data.Either
import Control.Exception.Base

type Discord = (RestChan, Gateway, [ThreadIdType])

startDiscord :: String -> (Discord -> IO ()) -> IO ()
startDiscord token =
  bracket
    (loginRestGateway (Auth (pack token)))
    stopDiscord

sendMessage :: Discord -> ChannelId -> Text -> IO (Either RestCallException Message)
sendMessage discord chan text =
  restCall discord $ CreateMessage chan text

nextCmd :: Discord -> ((Message, DiscordCmd) -> IO ()) -> IO ()
nextCmd discord handleCmd = do
  e <- nextEvent discord
  case e of
    Left err -> logStr (show err)
    Right (MessageCreate m) -> onMessageCreate m
    Right evt               -> return ()
  where
    onMessageCreate m = do
      chan <- restCall discord $ GetChannel (messageChannel m)
      case chan of
        Left err                      -> logStr (show err)
        Right ChannelDirectMessage {} -> if shouldIgnoreAuthor m then return ()
                                                                 else handleCmd (m, cmd m)
        _ -> return ()

    shouldIgnoreAuthor m =
      fromRight True (userIsBot <$> messageAuthor m)
