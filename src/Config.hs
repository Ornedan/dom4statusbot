{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Configuration.Utils hiding (Lens, Lens')
import Control.Lens (makeLenses)
import Control.Monad.Logger
import Data.Text
import Options.Applicative

data Config = Config
              { _ircServer       :: String
              , _ircNick         :: String
              , _ircChannel      :: String

              , _dbPath          :: String

              , _logPath         :: String
              , _logLevel        :: LogLevel

              , _gamePollThreads :: Int

              , _connectTimeout  :: Int
              , _pollTimeout     :: Int
              , _pollInterval    :: Int

              , _ggsPollEnabled  :: Bool
              , _ggsPollInterval :: Int
              } deriving (Show)

makeLenses ''Config

defaultConfig = Config
                { _ircServer       = ""
                , _ircNick         = "Treebot"
                , _ircChannel      = ""
                , _dbPath          = ":memory:"
                , _logPath         = ""
                , _logLevel        = LevelDebug
                , _gamePollThreads = 4
                , _connectTimeout  = 3
                , _pollTimeout     = 10
                , _pollInterval    = 60
                , _ggsPollEnabled  = False
                , _ggsPollInterval = 15 * 60
                }

instance FromJSON (Config -> Config) where
  parseJSON = withObject "Config" $
              \o -> id
                    <$< ircServer       ..: "ircServer" % o
                    <*< ircNick         ..: "ircNick" % o
                    <*< ircChannel      ..: "ircChannel" % o
                    <*< dbPath          ..: "dbPath" % o
                    <*< logPath         ..: "logPath" % o
                    <*< logLevel        ..: "logLevel" % o
                    <*< gamePollThreads ..: "gamePollThreads" % o
                    <*< connectTimeout  ..: "connectTimeout" % o
                    <*< pollTimeout     ..: "pollTimeout" % o
                    <*< pollInterval    ..: "pollInterval" % o
                    <*< ggsPollEnabled  ..: "ggsPollEnabled" % o
                    <*< ggsPollInterval ..: "ggsPollInterval" % o

instance ToJSON Config where
  toJSON a = object
    [ "ircServer"       .= _ircServer a
    , "ircNick"         .= _ircNick a
    , "ircChannel"      .= _ircChannel a
    , "dbPath"          .= _dbPath a
    , "logPath"         .= _logPath a
    , "logLevel"        .= _logLevel a
    , "gamePollThreads" .= _gamePollThreads a
    , "connectTimeout"  .= _connectTimeout a
    , "pollTimeout"     .= _pollTimeout a
    , "pollInterval"    .= _pollInterval a
    , "ggsPollEnabled"  .= _ggsPollEnabled a
    , "ggsPollInterval" .= _ggsPollInterval a
    ]
      
pConfig :: MParser Config
pConfig = id
          <$< ircServer .:: strOption
          % long "server"
          <> metavar "ADDRESS"
          <> help "IRC server address"
          
          <*< ircNick .:: strOption
          % long "nick"
          <> metavar "NAME"
          <> help "IRC nick"

          <*< ircChannel .:: strOption
          % long "channel"
          <> metavar "CHANNEL"
          <> help "IRC channel"

          <*< dbPath .:: strOption
          % long "db-path"
          <> metavar "PATH"
          <> help "database path"

          <*< logPath .:: strOption
          % long "log-path"
          <> metavar "PATH"
          <> help "log file path"

          <*< logLevel .:: pLogLevel
          % long "log-level"
          <> metavar "LEVEL"
          <> help "minimum level to log to file"

          <*< gamePollThreads .:: option auto
          % long "poll-threads"
          <> metavar "NUM"
          <> help "number of game server polling threads"
          
          <*< connectTimeout .:: option auto
          % long "connect-timeout"
          <> metavar "SECONDS"
          <> help "number of seconds to wait for server to respond"
          
          <*< pollTimeout .:: option auto
          % long "poll-timeout"
          <> metavar "SECONDS"
          <> help "TODO: what was this for anyway"
          
          <*< pollInterval .:: option auto
          % long "poll-interval"
          <> metavar "SECONDS"
          <> help "interval between automatic game status requests"
          
          <*< ggsPollEnabled .:: option auto
          % long "ggs-enabled"
          <> help "whether to get list of games from GGS"
          
          <*< ggsPollInterval .:: option auto
          % long "ggs-poll-interval"
          <> metavar "SECONDS"
          <> help "interval between games list updates"


-- Need to define these for LogLevel too

instance FromJSON LogLevel where
  parseJSON (String s)
    | toUpper s == "DEBUG" = return LevelDebug
    | toUpper s == "INFO"  = return LevelInfo
    | toUpper s == "WARN"  = return LevelWarn
    | toUpper s == "ERROR" = return LevelError
    | otherwise            = return (LevelOther s)

instance ToJSON LogLevel where
  toJSON LevelDebug = String "DEBUG"
  toJSON LevelInfo  = String "INFO"
  toJSON LevelWarn  = String "WARN"
  toJSON LevelError = String "ERROR"
  toJSON (LevelOther s) = String s

pLogLevel = option $ eitherReader $ \arg -> case toUpper $ pack arg of
  "DEBUG" -> return LevelDebug
  "INFO"  -> return LevelInfo
  "WARN"  -> return LevelWarn
  "ERROR" -> return LevelError
  _       -> return (LevelOther $ pack arg)
