{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Configuration.Utils
import Control.Lens
import Control.Logging
import Control.Concurrent
import Control.Monad
import Text.Printf
import Data.Text

import Config


mainInfo :: ProgramInfo Config
mainInfo = programInfo "Dominions 4 game tracker IRC bot" pConfig defaultConfig

main = runWithConfiguration mainInfo $ \conf -> do
  -- Set up logging
  -- TODO: Modify `logging` to support multiple sinks (with separate log levels)
  --       and log rotation (from fast-logger).
  setLogTimeFormat "%Y-%m-%d %H:%M:%S"
  let withLogging = case conf ^. logFile of
                     ""        -> withStderrLogging
                     otherwise -> withFileLogging (conf ^. logFile)
  
  withLogging $ do

    forM_ [1 .. 20 :: Int] $ \n -> do
      forkOS $ debugS "core" $ pack $ printf "derp %d" n
