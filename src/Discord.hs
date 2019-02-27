{-# LANGUAGE OverloadedStrings #-}

module Discord (alert) where

import Network.HTTP.Simple
import Data.String

alert :: String -> String -> IO ()
alert webhookUrl message = do

  let request = setRequestMethod "POST"
       $ setRequestHeader "Content-Type" ["application/json"]
       $ setRequestBodyLBS (fromString ("{\"content\":\"" ++ message ++ "\"}"))
       $ fromString webhookUrl

  _ <- httpNoBody request
  return ()
