module Main where

import Configuration.Utils
import Control.Logging

import Config


mainInfo :: ProgramInfo Config
mainInfo = programInfo "Dominions 4 game tracker IRC bot" pConfig defaultConfig

main = runWithConfiguration mainInfo $ \conf -> do
  -- Set up logging
  undefined

  -- Start components
  undefined

