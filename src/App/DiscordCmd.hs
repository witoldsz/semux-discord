{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module App.DiscordCmd where

import           ClassyPrelude
import           App.Semux
import           App.Types
import           App.Lib
import           Discord
import           Data.Maybe
import           Data.Either
import           Control.Exception.Base

data DiscordCmd
  = Hi
  | AddHelp
  | AddWallet !UserWallet
  | DelHelp
  | DelWallet !DelUserWalletRef
  | ListWallets
  | Unrecognized !Text
  deriving (Show)

data DelUserWalletRef = DelUserWalletRef
  { _duwAddr :: !Text
  , _duwUserId :: !UserId
  } deriving (Show)

cmd :: Message -> DiscordCmd
cmd m@Message {..} = case commandsToLower $ escDF <$> words messageText of
  ["add"]        -> AddHelp
  ["add", addr]  -> add [addr, shortAddr addr]
  ("add" : xs)   -> add xs
  ["del"]        -> DelHelp
  ["del", addr]  -> del addr
  ["list"]       -> ListWallets
  ["hi"]         -> Hi
  xs             -> Unrecognized $ unwords xs

  where
    commandsToLower :: [Text] -> [Text]
    commandsToLower =
      zipWith ($) (toLower : repeat id)

    add :: [Text] -> DiscordCmd
    add (addr : aliasWords)
      | isAddr addr = AddWallet $ UserWallet { _uwAddr   = addr
                                             , _uwAlias  = unwords aliasWords
                                             , _uwChanId = messageChannel
                                             , _uwUserId = authorOfThisMessage
                                             }
      | otherwise   = Unrecognized messageText

    del :: Text -> DiscordCmd
    del addr
      | isAddr addr = DelWallet $ DelUserWalletRef { _duwAddr = addr
                                                   , _duwUserId = authorOfThisMessage
                                                   }
      | otherwise   = Unrecognized messageText

    authorOfThisMessage :: UserId
    authorOfThisMessage =
      either (\_ -> Snowflake 0) userId messageAuthor
