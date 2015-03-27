{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Network.Polling
       (PollControl,
        PollResult(..),
        startPolling,
        stopPolling,
        requestPoll
       ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Logging
import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Network
import           Prelude hiding (catch)
import           System.IO
import           System.Timeout
import           Text.Printf

import           Config
import           Data.Time.Clock
import           Model.Game
import           Network.Protocol


type Address = (String, Int)
type InFlight = Map Address [TMVar PollResult]

data PollControl = PollControl
                   { pollThreads    :: [(ThreadId, TMVar ())]
                   , pollQueue      :: TQueue Address
                     -- TODO: better name
                   , inFlight       :: TVar InFlight
                     -- | Status updates stream - does not include failed polls.
                   , statusChan     :: TChan (GameStatus, UTCTime)
                   }

data PollResult = FailedOnException
                | ConnectingTimedOut
                | RequestTimedOut
                | Success
                  { receivedStatus :: GameStatus,
                    requestSent    :: UTCTime }
                deriving Show


startPolling :: Config -> IO PollControl
startPolling config = do
  -- Set up shared state
  pollQueue <- newTQueueIO
  inFlight <- newTVarIO Map.empty
  statusChan <- newTChanIO
  
  -- Start up polling threads
  pollKills <- forM [1 .. config ^. gamePollThreads] $ \n -> (n,) <$> newEmptyTMVarIO
  pollThreads <- forM pollKills $ \(nth, killSwitch) -> do
    let threadName = "polling-thread-" ++ show nth
    tid <- forkIO $ poller threadName config killSwitch pollQueue inFlight statusChan
    return (tid, killSwitch)
  
  return PollControl { pollThreads    = pollThreads
                     , pollQueue      = pollQueue
                     , inFlight       = inFlight
                     , statusChan     = statusChan
                     }

stopPolling :: PollControl -> IO ()
stopPolling control = do
  logS' "polling" "Toggling poll thread kill switches"
  forM_ (pollThreads control) $ \(tid, killSwitch) -> do
    atomically $ putTMVar killSwitch ()

requestPoll :: PollControl -> Address -> IO (TMVar PollResult)
requestPoll control address = do
  atomically $ do
    -- Only enqueue a new request if one is not already active (in queue or in progress)
    active <- Map.member address <$> readTVar (inFlight control)
    when (not active) $ do
      writeTQueue (pollQueue control) address
    -- But in either case we want to provide the caller with next poll result from
    -- the given address
    responseTMV <- newEmptyTMVar
    modifyTVar' (inFlight control) $ Map.insertWith (++) address [responseTMV]
    return responseTMV


poller :: String -> Config -> TMVar () -> TQueue Address -> TVar InFlight ->
          TChan (GameStatus, UTCTime) -> IO ()
poller threadName config killSwitch pollQueue inFlight statusChan = do
  logS' "polling" $ T.pack $ printf "%s: Polling thread starting" threadName
  loop
  where
    loop = do
      -- Note that we need to read both inputs in same `atomically` block so
      -- that kill switch toggle is noticed even if we're blocked on the queue.
      (live, next) <- atomically $ do
        live <- isEmptyTMVar killSwitch
        next <- if live then Just <$> readTQueue pollQueue else return Nothing
        return (live, next)
      -- Shutdown actions. Note it's first so loop is in tail position
      when (not live) $ do
        logS' "polling" $ T.pack $ printf "%s: Polling thread terminated" threadName

      when live $ do
        let address = fromJust next
        -- See if we can get a status update from the game
        result <- pollGame threadName config address
        atomically $ do
          -- Tell every requestor about the result
          requestorTMVs <- (Map.! address) <$> readTVar inFlight
          forM_ requestorTMVs $ \tmv -> putTMVar tmv result

          -- If we got a successfull result, also publish it to the status channel
          case result of
           Success {} -> writeTChan statusChan (receivedStatus result, requestSent result)
           _          -> return ()
          
          -- Someone can now enqueue the game to be polled again
          modifyTVar' inFlight $ Map.delete address
        
        -- And repeat
        loop


pollGame :: String -> Config -> Address -> IO PollResult
pollGame threadName config (host, port) = catch stageConnect $ \e -> do
  let err = show (e :: IOException)
  warnS' "polling" $ T.pack $ printf "%s: Polling %s:%d failed. Cause: %s" threadName host port err
  return FailedOnException
  
  where
    stageConnect = do
      debugS' "polling" $ T.pack $ printf "%s: Polling game at %s:%d" threadName host port

      let cto = config ^. connectTimeout * 1000 * 1000
      
      mhandle <- timeout cto $ connectTo host (PortNumber $ fromIntegral port)

      when (isNothing mhandle) $ do
        warnS' "polling" $ T.pack $ printf "%s: Connecting to %s:%d timed out" threadName host port

      maybe (return ConnectingTimedOut) stageRequest mhandle

    stageRequest :: Handle -> IO PollResult
    stageRequest handle = do
      let pto = config ^. pollTimeout * 1000 * 1000
      started <- getCurrentTime
      mstatus <- timeout (pto * 1000 * 1000) $ do
        status <- doGameStatusRequest handle
        hClose handle
        return status

      when (isNothing mstatus) $ do
        warnS' "polling" $ T.pack $ printf "%s: Game status request to %s:%d timed out" threadName host port

      maybe (return RequestTimedOut) (return . flip Success started) mstatus
  
