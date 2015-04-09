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
import           Network.RequestScheduling
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


startGGS :: Config -> RequestScheduleControl -> StorageControl -> IO GGSControl
startGGS conf reqSched storage = do
  killSwitch <- newEmptyTMVarIO

  tid <- forkIO $ ggsThread conf killSwitch reqSched storage

  return GGSControl { pollThread = tid
                    , killSwitch = killSwitch
                    }

stopGGS :: GGSControl -> IO ()
stopGGS control = do
  logS' "ggs" "Toggling GGS thread kill switch"
  atomically $ putTMVar (killSwitch control) ()


ggsThread :: Config -> TMVar () -> RequestScheduleControl -> StorageControl -> IO ()
ggsThread conf killSwitch reqSched storage = do
  logS' "ggs" "GGS poll thread starting"

  -- Heart with an initial beat waiting, so we do first update right away
  heart <- newTQueueIO
  atomically $ writeTQueue heart ()

  loop heart
  where
    beat heart = do
      threadDelay $ conf ^. ggsPollInterval * 1000 * 1000
      atomically $ writeTQueue heart ()
      
    loop heart = do
      -- Note that we need to read both inputs in same `atomically` block so
      -- that kill switch toggle is noticed even if we're blocked on the queue.
      (live, _ ) <- atomically $ do
        live <- isEmptyTMVar killSwitch
        beat <- if live then Just <$> readTQueue heart else return Nothing
        return (live, beat)
      
      when (not live) $ do
        logS' "ggs" "GGS poll thread terminated"

      when live $ do
        pollGGS reqSched storage

        -- Schedule next heartbeat
        forkIO $ beat heart
        loop heart

pollGGS :: RequestScheduleControl -> StorageControl -> IO ()
pollGGS reqSched storage = do
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
    handleNew reqSched storage new

    -- Process removed games
    handleRemoved storage gone


handleNew :: RequestScheduleControl -> StorageControl -> [GGSGame] -> IO ()
handleNew reqSched storage new = do
  forM_ new $ \g -> do
    logS' "ggs" $ T.pack $ printf "Found new game from GGS, tracking: %s:%d" (server g) (port g)

    -- Try to get the game's current status
    pollRes <- scheduleStatusRequest reqSched (server g, port g) >>= atomically . readTMVar

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
