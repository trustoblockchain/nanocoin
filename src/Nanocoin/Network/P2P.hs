{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.P2P (
  p2p,
) where

import Protolude hiding (msg, putText, print)
import Logger
import qualified System.Logger as Logger

import Control.Arrow ((&&&))

import Network.Socket (HostName, PortNumber)

import Nanocoin.Network.Node (NodeState, NodeConfig, NodeT, runNodeT)
import qualified Nanocoin.Block as Block
import qualified Nanocoin.MemPool as MemPool
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC

-------------------------------------------------------------------------------
-- P2P
-------------------------------------------------------------------------------

-- | Initializes a p2p node and returns a sender function so that
-- the rpc server can broadcast messages to the p2p network
p2p :: NodeState -> NodeConfig -> IO ()
p2p nodeState nodeConfig = do -- TODO: Take buffer size as argument, max size of blockchain
  let (sender,receiver) = Node.nodeSender &&& Node.nodeReceiver $ nodeState
  -- | Forever handle messages
  void $ forkIO $
    runNodeT nodeState nodeConfig $
      forever $ liftIO receiver >>=
        either Logger.logWarning (handleMsg . fst)

----------------------------------------------------------------
-- Msg Handling
----------------------------------------------------------------

-- | Main dispatch function to handle all messages received from network
handleMsg
  :: (MonadIO m, MonadLogger m)
  => Msg.Msg
  -> NodeT m ()
handleMsg msg = do

  logInfo $ "handleMsg: Received Msg: " <> (show msg :: Text)
  nodeSender <- gets Node.nodeSender

  case msg of

    Msg.QueryBlockMsg n -> do
      chain <- Node.getBlockChain
      case find ((==) n . Block.index) chain of
        Nothing -> logInfo ("handleMsg: No block with index " <> show n :: Text)
        Just block -> liftIO . nodeSender $ Msg.BlockMsg block

    Msg.BlockMsg block -> do
      mPrevBlock <- Node.getLatestBlock
      case mPrevBlock of
        Nothing -> logInfo "handleMsg: No Genesis block found."
        Just prevBlock -> do
          -- Apply block to world state
          Node.applyBlock prevBlock block
          -- If the block was successfully applied
          newPrevBlock <- Node.getLatestBlock
          when (Just block == newPrevBlock) $
            -- Ask if there is a more recent block
            liftIO . nodeSender $ Msg.QueryBlockMsg (Block.index block + 1)

    Msg.TransactionMsg tx -> do
      ledger <- Node.getLedger
      -- Verify Signature before adding to MemPool
      case T.verifyTxSignature ledger tx of
        Left err -> logError ("handleMsg: " <> show err :: Text)
        Right _  -> -- Add transaction to mempool
          Node.modifyMemPool_ $ MemPool.addTransaction tx
