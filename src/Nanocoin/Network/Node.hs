{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Nanocoin.Network.Node (
  NodeState(..),
  NodeConfig(..),
  NodeT,
  runNodeT,

  initNodeConfig,
  initNodeState,

  getNodeAddress,

  getBlockChain,
  modifyBlockChain_,
  setBlockChain,

  applyBlock,
  mineBlock,
  issueTransfer,
  getLatestBlock,

  getLedger,
  setLedger,

  getMemPool,
  modifyMemPool_,
  purgeMemPool,

) where

import Protolude hiding (print, putText)
import Logger
import qualified System.Logger as Logger

import Control.Distributed.Process
import Control.Distributed.Process.MonadBaseControl

import System.Console.Haskeline.MonadException (MonadException(..))

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..))
import qualified Data.Map as Map
import qualified Data.Text as T

import Address (Address)
import Nanocoin.Block (Block, Blockchain)
import Nanocoin.Transaction (Transaction)
import Nanocoin.MemPool (MemPool)

import qualified Address
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tx
import qualified Nanocoin.Ledger as Ledger
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Peer as Peer

import qualified Key

data NodeStateError
  = NoGenesisBlock
  | NoValidTxsInMemPool
  | InternalError Text
  deriving (Show)

-- Read-only Node config (context)
data NodeConfig = NodeConfig
  { host    :: Peer.HostName
  , p2pPort :: Peer.P2PPort
  , rpcPort :: Peer.RPCPort
  , keys    :: Key.KeyPair
  }

data NodeState = NodeState
  { nodeConfig   :: Peer.Peer -- XXX REMOVE (NodeConfig)
  , nodeChain    :: MVar Blockchain
  , nodeKeys     :: Key.KeyPair -- XXX REMOVE (NodeConfig)
  , nodeSender   :: Msg.MsgSender
  , nodeReceiver :: Msg.MsgReceiver
  , nodeLedger   :: MVar Ledger.Ledger
  , nodeMemPool  :: MVar MemPool
  }

type NodeT m = StateT NodeState (ReaderT NodeConfig m)

instance MonadException m => MonadException (NodeT m) where
  controlIO = controlIO

runNodeT :: MonadIO m => NodeState -> NodeConfig -> NodeT m a -> m a
runNodeT nodeState nodeConfig =
  flip runReaderT nodeConfig .
    flip evalStateT nodeState

-- Be able to log inside NodeT m a
instance MonadLogger m => MonadLogger (ReaderT NodeConfig m) where
  log lvl f = lift $ defaultLog lvl f

instance MonadLogger m => MonadLogger (StateT NodeState m) where
  log lvl f = lift $ defaultLog lvl f

-------------------------------------------------------------------------------
-- Node initialization
-------------------------------------------------------------------------------

initNodeConfig
  :: Peer.HostName      -- Host name to spawn Node on
  -> Peer.P2PPort       -- Port to listen to P2P Msgs on
  -> Peer.RPCPort       -- Port to run RPC server on
  -> Maybe Key.KeyPair  -- Node account keypair
  -> IO NodeConfig
initNodeConfig hn pport rport mkeys = do
  nodeKeys <- case mkeys of
    Nothing   -> Key.newKeyPair
    Just keys -> pure keys
  pure NodeConfig
    { host    = hn
    , p2pPort = pport
    , rpcPort = rport
    , keys    = nodeKeys
    }

initNodeState
  :: Peer.Peer
  -> Block
  -> Key.KeyPair
  -> IO NodeState
initNodeState self genesisBlock keys = do
  let (Peer.Peer hn p2pPort _) = self
  chainMV  <- newMVar [genesisBlock]
  (receiver, sender) <- M.initMulticast hn p2pPort 65536
  ledgerMV <- newMVar mempty
  memPoolMV <- newMVar mempty
  return NodeState
    { nodeConfig   = self
    , nodeChain    = chainMV
    , nodeKeys     = keys
    , nodeSender   = sender
    , nodeReceiver = receiver
    , nodeLedger   = ledgerMV
    , nodeMemPool  = memPoolMV
    }

-------------------------------------------------------------------------------
-- NodeState Updates
-------------------------------------------------------------------------------

modifyNodeState_
  :: MonadIO m
  => (NodeState -> MVar a) -- ^ NodeState field
  -> (a -> IO a)           -- ^ Modifying function
  -> NodeT m ()
modifyNodeState_ field f =
  liftIO . flip modifyMVar_ f =<< fmap field get

modifyBlockChain_
  :: MonadIO m
  => (Blockchain -> Block.Blockchain)
  -> NodeT m ()
modifyBlockChain_ f =
  modifyNodeState_ nodeChain (pure . f)

-- | Warning: Unsafe replace chain. Use 'setBlockChain' to safely update chain
setBlockChain
  :: MonadIO m
  => Blockchain
  -> NodeT m ()
setBlockChain chain = modifyBlockChain_ (const chain)

applyBlock
  :: (MonadIO m, MonadLogger m)
  => Block
  -> Block
  -> NodeT m ()
applyBlock prevBlock block = do
  ledger <- getLedger
  case Block.validateAndApplyBlock ledger prevBlock block of
    Left err -> logError err
    Right (ledger', itxs)
      | null itxs -> do
          logInfo "applyBlock: Block is valid. Applying block..."
          -- If no invalid transactions, add block to chain
          modifyBlockChain_ (block:)
          -- Remove stale, invalid transactions
          purgeMemPool
          -- Remove transactions in block from memPool
          let blockTxs = Block.transactions block
          modifyMemPool_ $ MP.removeTransactions blockTxs
          -- Update ledger to new ledger state
          setLedger ledger'
      | otherwise -> logWarning $ ("applyBlock:\n" <>) $
          T.unlines $ map ((<>) "\t" . show) itxs

mineBlock
  :: (MonadIO m, MonadLogger m)
  => NodeT m (Either NodeStateError Block)
mineBlock = do
  -- Attempt to mine block
  mPrevBlock <- getLatestBlock
  case mPrevBlock of
    Nothing -> pure $ Left NoGenesisBlock
    Just prevBlock -> do
      logInfo "mineBlock: Attempting to mine a block..."

      -- Validate and discard invalid transactions
      logInfo "mineBlock: Discarding Invalid Transactions..."
      invalidTxErrs <- purgeMemPool
      mapM_ logWarning invalidTxErrs
      validTxs <- MP.unMemPool <$> getMemPool

      -- Attempt to mine block with the valid transactions
      logInfo "mineblock: Constructing new block..."

      nodeKeys <- asks keys
      ledger <- getLedger

      if not (null validTxs)
        then do
          block <- Block.mineBlock prevBlock nodeKeys validTxs
          case Block.validateAndApplyBlock ledger prevBlock block of
            Left err -> pure $ Left $ InternalError (show err)
            Right (_, invalidTxErrs')
              | null invalidTxErrs' -> do
                  let blockHashText = decodeUtf8 (Block.hashBlock block)
                  logInfo $ "Generated block with hash:\n\t" <> blockHashText
                  -- Broadcast block message to network
                  p2pSender <- gets nodeSender
                  liftIO $ p2pSender (Msg.BlockMsg block)
                  pure $ Right block
              | otherwise -> panic $ -- This shouldn't happen, fail hard
                  T.intercalate "\n\t *" $
                    "Mined a block with invalid transactions:" :
                    map show invalidTxErrs'
         else
           pure $ Left NoValidTxsInMemPool

issueTransfer
  :: MonadIO m
  => Address
  -> Int
  -> NodeT m Transaction
issueTransfer toAddr amount = do
  nodeKeys <- asks keys
  tx <- liftIO $ Tx.transferTransaction nodeKeys toAddr amount
  p2pSender <- gets nodeSender
  liftIO . p2pSender $ Msg.TransactionMsg tx
  pure tx

setLedger
  :: (MonadIO m, MonadLogger m)
  => Ledger.Ledger
  -> NodeT m ()
setLedger ledger = do
  logInfo "setLedger: Updating Ledger..."
  modifyNodeState_ nodeLedger $ const $ pure ledger

modifyMemPool_
  :: MonadIO m
  => (MemPool -> MemPool)
  -> NodeT m ()
modifyMemPool_ f =
  modifyNodeState_ nodeMemPool (pure . f)

-- | Removes stale transactions, returning them
purgeMemPool
  :: MonadIO m
  => NodeT m [Tx.InvalidTx]
purgeMemPool = do
  ledger <- getLedger
  txs <- MP.unMemPool <$> getMemPool
  let (ledger', invalidTxErrs) = Tx.applyTransactions ledger txs
  let invalidTxs = map (\(Tx.InvalidTx tx _) -> tx) invalidTxErrs
  modifyMemPool_ $ MP.removeTransactions invalidTxs
  return invalidTxErrs

resetMemPool
  :: (MonadIO m, MonadLogger m)
  => NodeT m ()
resetMemPool = do
  logInfo "resetMemPool: Resetting memPool..."
  modifyMemPool_ (const mempty)

-------------------------------------------------------------------------------
-- NodeState Querying
-------------------------------------------------------------------------------

getNodeAddress :: MonadIO m => NodeT m Address
getNodeAddress = Address.deriveAddress . fst <$> asks keys

getBlockChain :: MonadIO m => NodeT m Blockchain
getBlockChain = readMVar' =<< fmap nodeChain get

getLatestBlock :: MonadIO m => NodeT m (Maybe Block)
getLatestBlock = head <$> getBlockChain

getLedger :: MonadIO m => NodeT m Ledger.Ledger
getLedger = readMVar' =<< fmap nodeLedger get

getMemPool :: MonadIO m => NodeT m MemPool
getMemPool = readMVar' =<< fmap nodeMemPool get

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => MVar a -> m a
readMVar' = liftIO . readMVar

getMVar :: MonadIO m => (a -> MVar b) -> a -> m b
getMVar f = readMVar' . f
