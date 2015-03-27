{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage
       (
         StorageControl,
         EntityNotExist(..),
         ListenResult(..),
         UnlistenResult(..),
         startStorage,
         stopStorage,
         listen,
         unlisten,
         startTracking,
         stopTracking,
         getTracked,
         listTracked,
         updateTracked

       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Logging
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Maybe
import Data.Time.Clock
import Database.Groundhog.Sqlite

import Config
import Model.Game
import Storage.Schema

data StorageControl = StorageControl
                      { accessThread :: ThreadId
                      , killSwitch   :: TMVar ()
                      , pendingOps   :: TQueue (Sqlite -> IO ())
                      }

data EntityNotExist = EntityNotExist
data EntityAlreadyExist = EntityAlreadyExist

data ListenResult = ListenAdded
                  | AlreadyListening
data UnlistenResult = ListenRemoved
                    | WasNotListening
data StartTrackingResult = StartedTracking
data StopTrackingResult = StoppedTracking


startStorage :: Config -> IO StorageControl
startStorage conf = do
  pendingOps <- newTQueueIO
  killSwitch <- newEmptyTMVarIO

  tid <- forkIO $ storageThread conf killSwitch pendingOps

  return StorageControl { accessThread = tid
                        , killSwitch = killSwitch
                        , pendingOps = pendingOps
                        }

stopStorage :: StorageControl -> IO ()
stopStorage control = do
  logS' "storage" "Toggling storage thread kill switch"
  atomically $ putTMVar (killSwitch control) ()

storageThread :: Config -> TMVar () -> TQueue (Sqlite -> IO ()) -> IO ()
storageThread conf killSwitch pendingOps = do
  logS' "storage" "Storage thread starting"

  withSqliteConn (conf ^. dbPath) $ \conn -> do
    flip runDbConn conn $ runMigration $ do
      migrate (undefined :: TrackedGame)
      migrate (undefined :: Listen)
    loop conn
  
  where
    loop conn = do
      -- Note that we need to read both inputs in same `atomically` block so
      -- that kill switch toggle is noticed even if we're blocked on the queue.
      (live, next) <- atomically $ do
        live <- isEmptyTMVar killSwitch
        next <- if live then Just <$> readTQueue pendingOps else return Nothing
        return (live, next)

      -- Shutdown actions. Note it's first so loop is in tail position
      when (not live) $ do
        logS' "storage" "Storage thread terminated"

      when live $ do
        let op = fromJust next
        op conn

        loop conn

-- Ops

listen :: StorageControl -> String -> String -> IO (Either EntityNotExist ListenResult)
listen control user gameRef = opBase control $ doListen user gameRef

unlisten :: StorageControl -> String -> String -> IO (Either EntityNotExist UnlistenResult)
unlisten control user gameRef = opBase control $ doUnlisten user gameRef

startTracking :: StorageControl -> String -> Int -> TrackMeta -> UTCTime -> GameStatus ->
                 IO (Either EntityAlreadyExist StartTrackingResult)
startTracking control host port meta pollTime initial =
  opBase control $ doStartTracking host port meta pollTime initial

stopTracking :: StorageControl -> String -> IO (Either EntityNotExist StopTrackingResult)
stopTracking control gameRef = opBase control $ doStopTracking gameRef

getTracked :: StorageControl -> String -> IO (Maybe TrackedGame)
getTracked control gameRef = opBase control $ doGetTracked gameRef

listTracked :: StorageControl -> IO [TrackedGame]
listTracked control = opBase control doListTracked

updateTracked :: StorageControl -> String -> Int -> UTCTime -> GameStatus -> IO ()
updateTracked control host port started status =
  opBase control $ doUpdateTracked host port started status


-- Op implementations

opBase :: StorageControl -> (TMVar a -> Sqlite -> IO ()) -> IO a
opBase control op = do
  resultTMV <- atomically newEmptyTMVar
  atomically $ writeTQueue (pendingOps control) $ op resultTMV
  atomically $ readTMVar resultTMV

doListen :: String -> String -> TMVar (Either EntityNotExist ListenResult) -> Sqlite -> IO ()
doListen user gameRef resultTMV conn = flip runDbConn conn $ do
  mgame <- findGameByRef gameRef

  result <- case mgame of
   Nothing   -> return $ Left EntityNotExist
   Just game -> do
     lkey <- insertByAll $ Listen user $ extractUnique game
     case lkey of
      Left  _ -> return $ Right AlreadyListening
      Right _ -> return $ Right ListenAdded
  
  liftIO $ atomically $ putTMVar resultTMV result

doUnlisten :: String -> String -> TMVar (Either EntityNotExist UnlistenResult) -> Sqlite ->
              IO ()
doUnlisten user gameRef resultTMV conn = flip runDbConn conn $ do
  mgame <- findGameByRef gameRef

  result <- case mgame of
   Nothing   -> return $ Left EntityNotExist
   Just game -> do
     let gkey :: Key TrackedGame (Unique TrackedGameAddress) = extractUnique game
     listens <- select (ListenerField ==. user &&. ListenToField ==. gkey)
     if null listens
       then return $ Right WasNotListening
       else do
       delete (ListenerField ==. user &&. ListenToField ==. gkey)
       return $ Right ListenRemoved

  liftIO $ atomically $ putTMVar resultTMV result

doStartTracking :: String -> Int -> TrackMeta -> UTCTime -> GameStatus ->
                   TMVar (Either EntityAlreadyExist StartTrackingResult) -> Sqlite -> IO ()
doStartTracking host port meta pollTime initial resultTMV conn = flip runDbConn conn $ do
  -- Are we already tracking this game?
  mgame <- getBy $ TrackedGameAddressKey host port

  -- Record it with current time for initial timestamp
  result <- case mgame of
    Just game -> return $ Left EntityAlreadyExist
    Nothing   -> do
      insert $ TrackedGame { host       = host
                           , port       = port
                           , meta       = meta
                           , lastPolled = pollTime
                           , lastStatus = initial
                           }
      return $ Right StartedTracking

  liftIO $ atomically $ putTMVar resultTMV result

doStopTracking :: String -> TMVar (Either EntityNotExist StopTrackingResult) -> Sqlite -> IO ()
doStopTracking gameRef resultTMV conn = flip runDbConn conn $ do
  -- Does the gameref correspond to any known game in the first place?
  mgame <- findGameByRef gameRef

  result <- case mgame of
    Nothing   -> return $ Left EntityNotExist
    Just game -> do
      delete (HostField ==. host game &&. PortField ==. port game)
      return $ Right StoppedTracking

  liftIO $ atomically $ putTMVar resultTMV result

doGetTracked :: String -> TMVar (Maybe TrackedGame) -> Sqlite -> IO ()
doGetTracked gameRef resultTMV conn = flip runDbConn conn $ do
  mgame <- findGameByRef gameRef
  liftIO $ atomically $ putTMVar resultTMV mgame

doListTracked :: TMVar [TrackedGame] -> Sqlite -> IO ()
doListTracked resultTMV conn = flip runDbConn conn $ do
  trackeds <- map snd <$> selectAll
  liftIO $ atomically $ putTMVar resultTMV trackeds

doUpdateTracked :: String -> Int -> UTCTime -> GameStatus -> TMVar () -> Sqlite -> IO ()
doUpdateTracked host port started status resultTMV conn = flip runDbConn conn $ do
  mgame <- select (HostField ==. host &&. PortField ==. port)
  
  case mgame of
   []        -> return ()
   [tracked] -> replaceBy TrackedGameAddress tracked { lastPolled = started,
                                                       lastStatus = status }
  
  liftIO $ atomically $ putTMVar resultTMV ()


findGameByRef :: PersistBackend m => String -> m (Maybe TrackedGame)
findGameByRef gameRef = do
  -- Try as host:port first. It's guaranteed not ambiguous
  byAddress <- maybe (return Nothing)
                     (\(host, port) -> getBy $ TrackedGameAddressKey host port)
                     (gameRefToAddress gameRef)

  -- Try as game name - if there are several games with same name, return one of them
  byName <- select (LastStatusField ~> NameSelector ==. gameRef)

  if isJust byAddress
    then return byAddress
    else if not $ null byName
         then return $ Just $ head byName
         else return Nothing

gameRefToAddress :: String -> Maybe (String, Int)
gameRefToAddress gameRef = do
  when (not $ ':' `elem` gameRef) $ do
    fail "No colon"

  let (host, ':':port) = break (== ':') gameRef

  when (any (not . isDigit) port) $ do
    fail "Port part is not a number"
  
  return (host, read port)
