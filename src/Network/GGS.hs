{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.GGS 
       ( GGSControl
       , startGGS
       , stopGGS
        ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Logging
import           Control.Monad
import           Data.Aeson (FromJSON, decode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           GHC.Generics
import           Network.HTTP hiding (host, port)
import           Network.URI
import           Text.Printf

import           Config
import           Model.Game
import           Network.Polling
import           Storage
import           Storage.Schema hiding (port)
import qualified Storage.Schema as Schema

data GGSControl = GGSControl
                  { pollThread :: ThreadId
                  , killSwitch :: TMVar ()
                  }

data GGSGame = GGSGame
               { server :: String
               , port   :: Int
               } deriving (Generic, Show)  

instance FromJSON GGSGame


startGGS :: Config -> PollControl -> StorageControl -> IO GGSControl
startGGS conf polling storage = do
  killSwitch <- newEmptyTMVarIO

  tid <- forkIO $ ggsThread conf killSwitch polling storage

  return GGSControl { pollThread = tid
                    , killSwitch = killSwitch
                    }

stopGGS :: GGSControl -> IO ()
stopGGS control = do
  logS' "ggs" "Toggling GGS thread kill switch"
  atomically $ putTMVar (killSwitch control) ()


ggsThread :: Config -> TMVar () -> PollControl -> StorageControl -> IO ()
ggsThread conf killSwitch polling storage = do
  logS' "ggs" "GGS poll thread starting"
  heart <- newTQueueIO

  loop heart
  where
    beat heart = do
      threadDelay $ conf ^. ggsPollInterval * 1000 * 1000
      atomically $ writeTQueue heart ()
      
    loop heart = do
      -- Schedule next heartbeat
      forkIO $ beat heart

      -- Note that we need to read both inputs in same `atomically` block so
      -- that kill switch toggle is noticed even if we're blocked on the queue.
      (live, beat) <- atomically $ do
        live <- isEmptyTMVar killSwitch
        beat <- if live then Just <$> readTQueue heart else return Nothing
        return (live, beat)

      when (not live) $ do
        logS' "ggs" "GGS poll thread terminated"

      when live $ do
        pollGGS polling storage
        loop heart

pollGGS :: PollControl -> StorageControl -> IO ()
pollGGS polling storage = do
  logS' "ggs" "Downloading current games list from GGS"

  mgames <- loadFromGGS

  when (isJust mgames) $ do
    let ggsGames = fromJust mgames
    tracked <- listTracked storage

    -- Determine which games GGS has we aren't tracking yet, and which we are
    -- tracking that are not in GGS.
    let ggsByAddr = Map.fromList $ map (\g -> ((server g, port g), g)) ggsGames 
    let trackedByAddr = Map.fromList $ map (\g -> ((host g, Schema.port g), g)) tracked
    
    let new = map snd $ Map.toList $ ggsByAddr `Map.difference` trackedByAddr
    let gone = map snd $ Map.toList $ trackedByAddr `Map.difference` ggsByAddr

    -- Process new games
    handleNew polling storage new

    -- Process removed games
    handleRemoved storage gone


handleNew :: PollControl -> StorageControl -> [GGSGame] -> IO ()
handleNew polling storage new = do
  forM_ new $ \g -> do
    logS' "ggs" $ T.pack $ printf "Found new game from GGS, tracking: %s:%d" (server g) (port g)

    -- Try to get the game's current status
    pollRes <- requestPoll polling (server g, port g) >>= atomically . readTMVar

    case pollRes of
     Success {} -> do
       debugS' "ggs" $ T.pack $ printf "Got initial status for %s:%d: %s" (server g) (port g) (show $ receivedStatus pollRes)
       startTracking storage
         (server g) (port g)
         (TrackMeta { source = GGS, announce = True })
         (requestSent pollRes) (receivedStatus pollRes)
       return ()
     -- Maybe it will be live later?
     _ -> return ()

handleRemoved :: StorageControl -> [TrackedGame] -> IO ()
handleRemoved storage gone = do
  forM_ gone $ \g -> do
    -- Only remove if we originally got it from GGS, too. It could otherwise be a live
    -- game that's just not tracked by GGS.
    when ((source $ meta g) == GGS) $ do
      logS' "ggs" $ T.pack $ printf "Game is gone from GGS, untracking: %s (%s:%d)" (name $ lastStatus g) (host g) (Schema.port g)
      void $ stopTracking storage $ printf "%s:%d" (host g) (Schema.port g)


request :: Request ByteString
request = replaceHeader HdrAccept "application/json, text/javascript, */*; q=0.01" $
          replaceHeader HdrAcceptEncoding "gzip, deflate" $
          replaceHeader (HdrCustom "X-Requested-With") "XMLHttpRequest" $
          mkRequest GET $ fromJust $ parseURI "http://www.brainwrinkle.net/"

loadFromGGS :: IO (Maybe [GGSGame])
loadFromGGS = do
  resp <- simpleHTTP request

  case resp of
   Left err -> do
     warnS' "ggs" $ T.pack $ printf "GGS download failed: " (show err)
     return Nothing
   Right _ -> do
     body <- getResponseBody resp
     case decode body of
      Nothing -> do
        warnS' "ggs" $ T.pack $ printf "Failed to decode games from: " (show resp)
        return Nothing
      Just games -> return $ Just games


{-
import Prelude hiding (log)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString(..))
import Data.Maybe
import Data.Set (Set)
import Database.Persist.Sqlite
import Network.HTTP
import Network.SimpleIRC
import Network.URI
import System.Log.Logger
import Text.Printf

import qualified Data.Set as Set

import Actions
import Config
import BotException
import Database
import DatabaseFlags
import GameInfo
import Util


instance FromJSON (String, Int) where  
  parseJSON (Object v) = (,) <$>
                         v .: "server" <*>
                         v .: "port"
  parseJSON _          = mzero


request :: Request ByteString
request = replaceHeader HdrAccept "application/json, text/javascript, */*; q=0.01" $
          replaceHeader HdrAcceptEncoding "gzip, deflate" $
          replaceHeader (HdrCustom "X-Requested-With") "XMLHttpRequest" $
          mkRequest GET $ fromJust $ parseURI "http://www.brainwrinkle.net/"


pollGGS :: IO [(String, Int)]
pollGGS = do
  resp <- simpleHTTP request >>= getResponseBody

  case decode resp of
    Nothing    -> failMsg $ "Failed to decode games from: " ++ (show resp)
    Just games -> return $ map (toLowercase *** id) games


ggsLoop :: ActionState -> MIrc -> IO ()
ggsLoop baseState irc = do
  let state    = baseState { sIrc = irc }
      interval = fromIntegral $ cGGSPollInterval $ sConfig baseState
  
  forever $ flip runReaderT state $ do
    log INFO $ printf "Polling GGS's games list"
    -- Get currently known games from DB and GGS
    dbGames <- runDB $ selectList [] []
    mggsGames <- pollGGS'
    
    when (isJust mggsGames) $ do
      let ggsGames = fromJust mggsGames
          dbSet   = Set.fromList $ map ((gameHost &&& gamePort) . entityVal) dbGames
          ggsSet  = Set.fromList ggsGames
          -- Games known by GGS and not by us
          added   = Set.toList $ ggsSet Set.\\ dbSet
          -- Games known by us, but not by GGS. Note that these might also just be games
          -- not registered on GGS.
          removed = Set.toList $ dbSet Set.\\ ggsSet
      
      forM_ added add
      forM_ removed remove
    
    delay interval
  
  where
    pollGGS' = liftIO (pollGGS >>= return . Just)
               `caughtAction`
               (\msg -> do
                   when (not $ null msg) $ log WARNING $ printf "pollGGS: Polling failed: %s"  msg
                   return Nothing)
               
    add (host, port) =
      add' host port
      `caughtAction`
      (\msg -> when (not $ null msg) $ log WARNING $ printf "pollGGS: Failed to add game (%s:%d): %s" host port msg)
    add' host port = do
      ent <- runDB $ getBy (Address host port)
      when (isNothing ent) $ do
        now <- getTime
        game <- requestGameInfo host port
        
        runDB $ insert $ Game host port GGS [] now (toLowercase $ name game) game
        
        let msg = printf "Added game %s from GGS (%s:%d)" (name game) host port
        log NOTICE msg
        announce msg
    
    remove (host, port) =
      remove' host port
      `caughtAction`
      (\msg -> when (not $ null msg) $ log WARNING $ printf "pollGGS: Failed to remove game (%s:%d): %s" host port msg)
    remove' host port = do
      let address = Address host port
      ment <- runDB $ getBy address
      
      -- Only remove games that were added from GGS here. Anything added
      -- manually needs to be removed manually.
      when (isJust ment) $ do
        let ent = fromJust ment
        when (gameSource (entityVal ent) == GGS) $ do
          runDB $ do
            deleteWhere [ListenGame ==. entityKey ent]
            deleteBy address
          
          let msg = printf "Removed game %s (%s:%d)" (name $ gameGameInfo $ entityVal ent) host port
          log NOTICE msg
          announce msg
-}
