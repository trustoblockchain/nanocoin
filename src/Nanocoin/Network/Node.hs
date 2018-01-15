{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Nanocoin.Network.Node (
  NodeEnv(..),
  NodeConfig(..),
  NodeStateError(..),

  NodeT,
  runNodeT,

  NodeProcessM,
  runNodeProcessM,

  initNodeConfig,
  initNodeState,

  getNodeAddress,
  getNodeKeys,

  getBlockChain,
  modifyBlockChain_,
  setBlockChain,

  applyBlock,
  mineBlock,

  getLatestBlock,

  getLedger,
  setLedger,

  getMemPool,
  modifyMemPool_,
  purgeMemPool,

  getPeers,
  addPeer,
  removePeer,

  nsendAllPeers,

) where

import Protolude hiding (log)
import Logger

import Control.Monad.Trans
import Control.Monad.Trans.Control

import Control.Monad.Base
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Distributed.Process.Serializable

import System.Console.Haskeline.MonadException (MonadException(..))

import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON(..))
import qualified Data.Map as Map
import qualified Data.Serialize as S
import qualified Data.Set as Set
import qualified Data.Text as T

import Address (Address)
import Nanocoin.Block (Block, Blockchain)
import Nanocoin.Transaction (Transaction)
import Nanocoin.MemPool (MemPool)
import Nanocoin.Network.Peer (Peer(..), Peers)
import Nanocoin.Network.Service (Service(..))

import qualified Address
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tx
import qualified Nanocoin.Ledger as Ledger
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.Utils as Utils

import qualified Key

data NodeStateError
  = NoGenesisBlock
  | NoValidTxsInMemPool
  | InternalError Text
  deriving (Show)

data NodeConfig = NodeConfig
  { host    :: Utils.HostName
  , p2pPort :: Utils.P2PPort
  , rpcPort :: Utils.RPCPort
  , keys    :: Key.KeyPair
  }

data NodeState = NodeState
  { nodeChain    :: MVar Blockchain
  , nodePeers    :: MVar Peers
  , nodeLedger   :: MVar Ledger.Ledger
  , nodeMemPool  :: MVar MemPool
  }

data NodeEnv = NodeEnv
  { nodeConfig :: NodeConfig
  , nodeState  :: NodeState
  }

newtype NodeT m a = NodeT { unNodeT :: ReaderT NodeEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader NodeEnv, MonadException)

runNodeT :: NodeEnv -> NodeT m a -> m a
runNodeT nodeEnv = flip runReaderT nodeEnv . unNodeT

-------------------------------------------------------------------------------

deriving instance MonadBase IO m => MonadBase IO (NodeT m)

instance MonadTransControl NodeT where
  type StT NodeT a = StT (ReaderT NodeEnv) a
  liftWith = defaultLiftWith NodeT unNodeT
  restoreT = defaultRestoreT NodeT

instance MonadBaseControl IO m => MonadBaseControl IO (NodeT m) where
  type StM (NodeT m) a = ComposeSt NodeT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadProcess m => MonadProcess (NodeT m) where
  liftP = NodeT . liftP

instance MonadProcessBase m => MonadProcessBase (NodeT m) where
  type StMP (NodeT m) a = ComposeStP NodeT m a
  liftBaseWithP = defaultLiftBaseWithP
  restoreMP = defaultRestoreMP

instance MonadLogger m => MonadLogger (NodeT m) where
  log lvl f = lift $ log lvl f

type NodeProcessM = NodeT (LoggerT Process)

runNodeProcessM :: Logger -> NodeEnv -> NodeProcessM a -> Process a
runNodeProcessM logger nodeEnv =
  runLoggerT logger . runNodeT nodeEnv

-------------------------------------------------------------------------------
-- Node initialization
-------------------------------------------------------------------------------

initNodeConfig
  :: Utils.HostName      -- Host name to spawn Node on
  -> Utils.P2PPort       -- Port to listen to P2P Msgs on
  -> Utils.RPCPort       -- Port to run RPC server on
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
  :: Block
  -> IO NodeState
initNodeState genesisBlock = do
  chainMV   <- newMVar [genesisBlock]
  peersMV   <- newMVar mempty
  ledgerMV  <- newMVar mempty
  memPoolMV <- newMVar mempty
  return NodeState
    { nodeChain   = chainMV
    , nodePeers   = peersMV
    , nodeLedger  = ledgerMV
    , nodeMemPool = memPoolMV
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
  liftIO . flip modifyMVar_ f =<<
    fmap field (asks nodeState)

modifyBlockChain_
  :: MonadIO m
  => (Blockchain -> Block.Blockchain)
  -> NodeT m ()
modifyBlockChain_ f =
  modifyNodeState_ nodeChain (pure . f)

setBlockChain
  :: MonadIO m
  => Blockchain
  -> NodeT m ()
setBlockChain chain =
  modifyBlockChain_ (const chain)

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

-- | Reset the mempool such that it no longer contains any transactions
resetMemPool
  :: (MonadIO m, MonadLogger m)
  => NodeT m ()
resetMemPool = do
  logInfo "resetMemPool: Resetting memPool..."
  modifyMemPool_ (const mempty)

addPeer
  :: MonadIO m
  => Peer
  -> NodeT m ()
addPeer peer =
  modifyNodeState_ nodePeers (pure . Set.insert peer)

removePeer
  :: MonadIO m
  => Peer
  -> NodeT m ()
removePeer peer =
  modifyNodeState_ nodePeers (pure . Set.delete peer)

-------------------------------------------------------------------------------
-- Node Actions
-------------------------------------------------------------------------------

applyBlock
  :: (MonadIO m, MonadLogger m)
  => Block
  -> Block
  -> NodeT m ()
applyBlock prevBlock block = do
  ledger <- getLedger
  case Block.validateAndApplyBlock ledger prevBlock block of
    Left err -> lift $ logError err
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

      nodeKeys <- getNodeKeys
      ledger <- getLedger

      if not (null validTxs)
        then do
          block <- Block.mineBlock prevBlock nodeKeys validTxs
          case Block.validateAndApplyBlock ledger prevBlock block of
            Left err -> pure $ Left $ InternalError (show err)
            Right (_, invalidTxErrs')
              | null invalidTxErrs' -> do
                  let blockHashText = decodeUtf8 (Block.hashBlock block)
                  logInfo $ "mineBlock: Generated block with hash:\n\t" <> blockHashText
                  pure $ Right block
              | otherwise -> panic $ -- This shouldn't happen, fail hard
                  T.intercalate "\n\t *" $
                    "Mined a block with invalid transactions:" :
                    map show invalidTxErrs'
         else
           pure $ Left NoValidTxsInMemPool


-------------------------------------------------------------------------------
-- Node Querying
-------------------------------------------------------------------------------

getNodeConfig :: Monad m => NodeT m NodeConfig
getNodeConfig = asks nodeConfig

getNodeState :: Monad m => NodeT m NodeState
getNodeState = asks nodeState

getNodeAddress :: Monad m => NodeT m Address
getNodeAddress = Address.deriveAddress . fst . keys <$> getNodeConfig

getBlockChain :: MonadIO m => NodeT m Blockchain
getBlockChain = readMVar' =<< fmap nodeChain getNodeState

getLatestBlock :: MonadIO m => NodeT m (Maybe Block)
getLatestBlock = head <$> getBlockChain

getLedger :: MonadIO m => NodeT m Ledger.Ledger
getLedger = readMVar' =<< fmap nodeLedger getNodeState

getMemPool :: MonadIO m => NodeT m MemPool
getMemPool = readMVar' =<< fmap nodeMemPool getNodeState

getPeers :: MonadIO m  => NodeT m Peers
getPeers = readMVar' =<< fmap nodePeers getNodeState

getNodeKeys :: MonadIO m => NodeT m Key.KeyPair
getNodeKeys = keys <$> getNodeConfig

--------------------------------------------------------------------------------
-- Messaging
--------------------------------------------------------------------------------

nsendAllPeers
  :: (MonadIO m, MonadProcess m, S.Serialize a)
  => Service
  -> a
  -> NodeT m ()
nsendAllPeers service msg = do
  peers <- getPeers
  forM_ (Set.toList peers) $ \peer ->
    nsendPeer peer service msg

nsendPeer
  :: (MonadIO m, MonadProcess m, S.Serialize a)
  => Peer
  -> Service
  -> a
  -> NodeT m ()
nsendPeer (Peer pid) service msg =
  nsendRemote (processNodeId pid) (show service) (S.encode msg)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => MVar a -> m a
readMVar' = liftIO . readMVar

getMVar :: MonadIO m => (a -> MVar b) -> a -> m b
getMVar f = readMVar' . f
