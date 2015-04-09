{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Storage.Schema where

import Data.Time
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

import Model.Dominions4
import Model.Game


data TrackSource = Manual
                   { trackRequestor :: String
                   }
                 | GGS
                 deriving (Eq, Read, Show)

data TrackMeta = TrackMeta
                 { source   :: TrackSource
                 , announce :: Bool
                 } deriving (Show)

data TrackedGame = TrackedGame
                   { host       :: String
                   , port       :: Int
                   , meta       :: TrackMeta
                   , lastPolled :: UTCTime
                   , lastStatus :: GameStatus
                   } deriving (Show)

data TrackedGameAddress v where
  TrackedGameAddress :: TrackedGameAddress (UniqueMarker TrackedGame)

data Listen = Listen
              { listener :: String
              , listenTo :: Key TrackedGame (Unique TrackedGameAddress)
              }
deriving instance Show Listen


-- Database definitions for everything
mkPersist defaultCodegenConfig [groundhog|
- entity: TrackedGame
  autoKey: null
  keys:
    - name: TrackedGameAddress
      default: true
  constructors:
    - name: TrackedGame
      uniques:
        - name: TrackedGameAddress
          type: primary
          fields: [host, port]
- entity: Listen
  constructors:
    - name: Listen
      uniques:
        - name: UniqueListen
          fields: [listener, listenTo]
- embedded: GameStatus
  dbName: GameStatus
- embedded: TrackMeta
- primitive: TrackSource
  representation: showread
- entity: PlayerStatus
- entity: ModInfo
- primitive: Era
  representation: showread
- primitive: GameState
  representation: showread
- primitive: PlayerType
  representation: showread
- primitive: SubmissionType
  representation: showread
|]

{-
main = withSqliteConn ":memory:" $ \conn -> do
  print "meh"

  flip runDbConn conn $ runMigration $ do
    migrate (undefined :: TrackedGame)
    migrate (undefined :: Listen)
  
  print "feh"
-}
