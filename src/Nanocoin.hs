{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin (
  initNode
) where

import Protolude hiding (get, put)

import Control.Concurrent.Chan
import Control.Distributed.Process.Lifted (NodeId(..))

import qualified Data.Set as Set

import Logger
import qualified Key
import qualified Nanocoin.Block as B
import qualified Nanocoin.CLI as CLI
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.P2P as P2P
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC
import qualified Nanocoin.Network.Utils as Utils

-- | Initializes a node on the network with it's own copy of
-- the blockchain, and invokes a p2p server and an http server.
initNode
  :: Utils.RPCPort
  -> Utils.P2PPort
  -> Maybe FilePath
  -> Logger
  -> IO ()
initNode rpcPort p2pPort mKeysPath logger = do

  -- Initialize Node Keys
  keys <- case mKeysPath of
    Nothing -> Key.newKeyPair
    Just keysPath -> do
      eNodeKeys <- Key.readKeys keysPath
      case eNodeKeys of
        Left err   -> die $ show err
        Right keys -> pure keys

  -- Initialize Genesis Block
  genesisBlock <- do
    eKeys <- Key.readKeys "keys/genesis"
    case eKeys of
      Left err   -> die $ show err
      Right gkeys -> B.genesisBlock gkeys

  -- Initialize NodeState & NodeConfig
  nodeState  <- Node.initNodeState genesisBlock
  -- XXX remove hardcoded values, get from config file
  let theHost = "127.0.1.1"
  nodeConfig <- Node.initNodeConfig theHost p2pPort rpcPort (Just keys)
  let nodeEnv = Node.NodeEnv nodeConfig nodeState

  -- XXX remove hardcoded values, get from config file
  node1Id <- Utils.mkNodeId "127.0.1.1" 8001
  node2Id <- Utils.mkNodeId "127.0.1.1" 8002
  -- add current node to the boot nodes
  thisNodeId <- Utils.mkNodeId theHost p2pPort
  let bootnodes = [node1Id, node2Id, thisNodeId]

  -- Init chan to send Msgs from
  -- rpc & console proc to p2p network
  cmdChan <- newChan

  -- Fork RPC server
  forkIO $
    RPC.rpcServer
      logger
      nodeEnv
      cmdChan

  -- Fork P2P server
  forkIO $
    P2P.bootstrap
      logger
      nodeEnv
      cmdChan
      bootnodes

  -- Run cmd line interface
  CLI.cli logger nodeEnv cmdChan
