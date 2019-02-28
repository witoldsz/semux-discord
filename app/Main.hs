{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Semux (getLastCoinbase)
import DiscordHook (alert)
import System.Environment
import Data.Time.Clock

main :: IO ()
main = do
  semuxApi <- getEnv "SEMUX_API"
  delegate <- getEnv "DELEGATE"
  webhookUrl <- getEnv "WEBHOOK_URL"
  alertAfterSecs <- read <$> getEnv "ALERT_AFTER_SECS" :: IO Int

  lastCoinbase <- getLastCoinbase semuxApi delegate
  now <- getCurrentTime
  let diff = diffSeconds now lastCoinbase

  if (diff > alertAfterSecs)
    then do
      let msg = alertMessage delegate lastCoinbase diff
      alert webhookUrl msg
      putStrLn msg
    else
      putStrLn $ "Last COINBASE was " ++ show lastCoinbase ++ " that is " ++ show diff ++ " seconds ago. OK."

diffSeconds :: UTCTime -> UTCTime -> Int
diffSeconds t1 t2 =
  let (res, _) = properFraction $ diffUTCTime t1 t2
  in res

alertMessage :: String -> UTCTime -> Int -> String
alertMessage delegate lastCoinbase diff =
  "Alert! `"
    ++ delegate
    ++ "` hasn't forged since `"
    ++ show lastCoinbase
    ++ "` (i.e. "
    ++ show diff
    ++ " seconds ago). This isn't good."
