{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import App.App
import System.Environment

main :: IO ()
main = do
  semuxApiUrl <- getEnv "SEMUX_API"
  dicordSecret <- getEnv "DISCORD_SECRET"
  app Configuration{..}
