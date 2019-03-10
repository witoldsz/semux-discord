{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import App.App
import System.Environment

main :: IO ()
main = do
  _semuxApiUrl <- getEnv "SEMUX_API"
  _dicordSecret <- getEnv "DISCORD_SECRET"
  app Configuration{..}
