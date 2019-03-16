{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.DiscordCmd where

import           ClassyPrelude
import           App.Semux
import           App.Db
import           App.Lib
import           Discord
import qualified Data.Text as T
import           Data.Maybe
import           Data.Either
import           Control.Exception.Base

data DiscordCmd
  = Hi
  | AddWallet UserWallet
  | AddHelp
  | DelWallet !Text
  | Unrecognized !Text

cmd :: Message -> DiscordCmd
cmd m@Message {..} = case commandsToLower (T.words messageText) of
  ("add" : xs)  -> add xs
  ["del", addr] -> DelWallet addr
  ["hi"]        -> Hi
  _             -> Unrecognized messageText

  where
    commandsToLower =
      zipWith ($) (T.toLower : repeat id)

    add args =
      case args of
        [addr]          -> add [addr, shortAddr addr]
        (addr : aliass)
          | isAddr addr -> AddWallet $ UserWallet { _uwAddr   = addr
                                                  , _uwAlias  = T.intercalate " " aliass
                                                  , _uwChanId = messageChannel
                                                  , _uwUserId = authorOf m
                                                  }
          | otherwise   -> AddHelp
        _               -> AddHelp

authorOf :: Message -> UserId
authorOf Message {..} =
  either (\_ -> Snowflake 0) userId messageAuthor
