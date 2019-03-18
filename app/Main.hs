{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import App.App
import System.Environment
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr NoBuffering

  semuxApiUrl <- getEnv "SEMUX_API"
  dicordSecret <- getEnv "DISCORD_SECRET"

  app Configuration{..}
