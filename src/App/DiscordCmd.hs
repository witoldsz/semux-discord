{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.DiscordCmd where

import           ClassyPrelude
import           App.Semux
import           App.Db
import           App.Lib
import           Discord
import           Data.Text
import           Data.Maybe
import           Data.Either
import           Control.Exception.Base

data DiscordCmd
  = Hi
  | AddWallet UserWallet
  | AddHelp
  | DelWallet !Text
  | Unrecognized Message

cmd :: Message -> DiscordCmd
cmd m@Message {..} = case Data.Text.words messageText of
  -- TODO: validate addr
  ("add" : addr : xs) | isAddr addr -> AddWallet $ UserWallet { _uwAddr   = addr
                                                , _uwAlias  = fromMaybe (shortAddr addr) $ headMay xs
                                                , _uwChanId = messageChannel
                                                , _uwUserId = authorOf m
                                                }
  ["add"]       -> AddHelp
  ["del", addr] -> DelWallet addr
  ["hi"]        -> Hi
  _             -> Unrecognized m

authorOf :: Message -> UserId
authorOf Message {..} =
  either (\_ -> Snowflake 0) userId messageAuthor
