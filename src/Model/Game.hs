{-# LANGUAGE DeriveGeneric #-}
module Model.Game where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.Maybe
import GHC.Generics

import Model.Dominions4


data GameStatus = GameStatus
                  { name       :: String
                  , state      :: GameState
                  , turn       :: Int
                  , timeToHost :: Int
                  , era        :: (Maybe Era)
                  , nations    :: [PlayerStatus]
                  , mods       :: [ModInfo]
                  } deriving (Eq, Generic, Show)

data GameState = Waiting
               | Running
               deriving(Enum, Eq, Read, Show)

data PlayerStatus = PlayerStatus
                    { nationId  :: Int
                    , player    :: PlayerType
                    , submitted :: SubmissionType
                    , connected :: Bool
                    } deriving (Eq, Generic, Show)

data PlayerType = Empty
                | Human
                | AI
                | Closed
                | DefeatedThisTurn
                | DefeatedEarlier
                deriving (Enum, Eq, Read, Show)

data SubmissionType = None
                    | Partial
                    | Full
                    deriving (Enum, Eq, Read, Show)

data ModInfo = ModInfo
               { modName         :: String
               , modMajorVersion :: Int
               , modMinorVersion :: Int
               } deriving (Eq, Generic, Show)

instance NFData GameStatus where rnf = genericRnf
instance NFData ModInfo where rnf = genericRnf
instance NFData PlayerStatus where rnf = genericRnf

instance NFData Era
instance NFData GameState
instance NFData PlayerType
instance NFData SubmissionType
