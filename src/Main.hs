{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Configuration.Utils
import           Control.Concurrent
import           Control.Lens
import           Control.Logging
import           Control.Monad
import qualified Data.Text as T
import           Text.Printf

import           Config
import           Network.GGS
import           Network.Polling
import           Storage


mainInfo :: ProgramInfo Config
mainInfo = programInfo "Dominions 4 game tracker IRC bot" pConfig defaultConfig

main = runWithConfiguration mainInfo $ \conf -> do
  -- Set up logging
  -- TODO: Modify `logging` to support multiple sinks (with separate log levels)
  --       and log rotation (from fast-logger).
  setLogTimeFormat "%Y-%m-%d %H:%M:%S"
  let withLogging = case conf ^. logPath of
                     ""        -> withStderrLogging
                     otherwise -> withFileLogging (conf ^. logPath)
  
  withLogging $ do
    -- Start components
    pollControl    <- startPolling conf
    storageControl <- startStorage conf
    ggsControl     <- startGGS conf pollControl storageControl

    undefined
