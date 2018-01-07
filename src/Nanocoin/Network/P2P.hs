{-# language TupleSections #-}
{-# language FlexibleContexts #-}

module Nanocoin.Network.P2P (
  p2p,
) where

import Protolude

import Control.Monad.Base
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Distributed.Process.Node.Lifted

import qualified Data.Set as Set

import Network.Socket (ServiceName)
import Network.Transport.TCP

import Nanocoin.Network.Peer    (Peer(..), Peers)
import Nanocoin.Network.Service (Service(..))
import Nanocoin.Network.Utils   (HostName)
import qualified Nanocoin.Network.Node as Node
import Logger (Logger, runLoggerT, logWarning, logInfo)

p2p
  :: Logger
  -> Node.NodeEnv
  -> [NodeId]
  -> IO ()
p2p logger nodeEnv bootnodes = do

    let hostname = Node.host $ Node.nodeConfig nodeEnv
    let p2pPort  = Node.p2pPort $ Node.nodeConfig nodeEnv

    -- Create a local node to run processes on
    eLocalNode <- createLocalNode hostname $ show p2pPort
    case eLocalNode of
      Left err -> Protolude.die err
      Right localNode -> do

        -- Initialize P2P controller process
        forkProcess localNode $
          runNodeProcessM $ p2pController bootnodes

        -- Wait for P2P Controller to boot and discover peers
        runProcess localNode $
          runNodeProcessM $
            waitP2PController $ do
              forever $ do
                liftBase $ threadDelay 3000000
                print =<< Node.getPeers

  where
    runNodeProcessM =
      runLoggerT logger . Node.runNodeT nodeEnv

--------------------------------------------------------------------------------
-- P2P Controller Process
--------------------------------------------------------------------------------

p2pController :: [NodeId] -> Node.NodeProcessM ()
p2pController bootnodes = do
  pid <- getSelfPid
  register (show PeerDiscovery) pid

  -- Discover peers in the network
  -- Note: This can't go outside of this function (why?)
  mapM_ discoverPeer bootnodes

  controlP $ \runInProc ->
    forever $ receiveWait
      [ match $ runInProc . onPeerReply
      , match $ runInProc . onMonitorNotif
      ]

waitP2PController :: Node.NodeProcessM () -> Node.NodeProcessM ()
waitP2PController proc = do
  mPid <- whereis (show PeerDiscovery)
  case mPid of
    Nothing -> do
      logWarning "Could not connect to PeerDiscovery process. Retrying..."
      liftBase $ threadDelay 1000000
      waitP2PController proc
    Just pid -> do
      logInfo $ "Found PeerDiscovery process. " <> (show pid :: Text)
      proc

--------------------------------------------------------------------------------
-- P2P Message Handlers
--------------------------------------------------------------------------------

isPeerReply :: WhereIsReply -> Bool
isPeerReply (WhereIsReply nm pid) =
  nm == (show PeerDiscovery) && isJust pid

-- | Add the new process to the peers list and monitor it's process
onPeerReply :: WhereIsReply -> Node.NodeProcessM ()
onPeerReply (WhereIsReply _ mPid) = do
  putText $ "Recieved WhereIsReply: " <> show mPid
  case mPid of
    Nothing -> pure ()
    Just pid -> do
      peers <- Node.getPeers
      let peer = Peer pid
      unless (peer `Set.member` peers) $ do
        Node.addPeer peer
        void $ monitor pid

-- | Remove a peer from the peers list and unmonitor the process
onMonitorNotif :: ProcessMonitorNotification -> Node.NodeProcessM ()
onMonitorNotif (ProcessMonitorNotification mref pid _) = do
  unmonitor mref
  peers <- Node.getPeers
  let peer = Peer pid
  when (peer `Set.member` peers) $ do
    Node.removePeer peer

--------------------------------------------------------------------------------
-- P2P Utils
--------------------------------------------------------------------------------

type P2PPort = ServiceName

-- | Send a 'whereis' message to a node's P2PDiscovery process.
-- Replies come in  the form of a WhereIsReply message.
discoverPeer :: NodeId -> Node.NodeProcessM ()
discoverPeer nid = do
  putText $ "Pinging: " <> show nid
  whereisRemoteAsync nid (show PeerDiscovery)

createLocalNode
  :: HostName
  -> P2PPort
  -> IO (Either Text LocalNode)
createLocalNode host port = do
  eTransport <- createTransport host port defaultTCPParameters
  case eTransport of
    Left err -> pure $ Left $ show err
    Right transport -> Right <$>
      newLocalNode transport initRemoteTable
