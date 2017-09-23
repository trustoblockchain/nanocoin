{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Node (
  NodeState(..),

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

data NodeState = NodeState
  { nodeConfig   :: Peer.Peer
  , nodeChain    :: MVar Blockchain
  , nodeKeys     :: Key.KeyPair
  , nodeSender   :: Msg.MsgSender
  , nodeReceiver :: Msg.MsgReceiver
  , nodeLedger   :: MVar Ledger.Ledger
  , nodeMemPool  :: MVar MemPool
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
  => NodeState             -- ^ NodeState
  -> (NodeState -> MVar a) -- ^ NodeState field
  -> (a -> IO a)           -- ^ Modifying function
  -> m ()
modifyNodeState_ nodeState f = liftIO . modifyMVar_ (f nodeState)

modifyBlockChain_
  :: MonadIO m
  => NodeState
  -> (Blockchain -> Block.Blockchain)
  -> m ()
modifyBlockChain_ nodeState f = modifyNodeState_ nodeState nodeChain (pure . f)

-- | Warning: Unsafe replace chain. Use 'setBlockChain' to safely update chain
setBlockChain :: MonadIO m => NodeState -> Blockchain -> m ()
setBlockChain nodeState chain = modifyBlockChain_ nodeState (const chain)

applyBlock
  :: (MonadIO m, MonadLogger m)
  => NodeState
  -> Block
  -> Block
  -> m ()
applyBlock nodeState prevBlock  block = do
  ledger <- getLedger nodeState
  case Block.validateAndApplyBlock ledger prevBlock block of
    Left err -> logError err
    Right (ledger', itxs)
      | null itxs -> do
          logInfo "applyBlock: Block is valid. Applying block..."
          -- If no invalid transactions, add block to chain
          modifyBlockChain_ nodeState (block:)
          -- Remove stale, invalid transactions
          purgeMemPool nodeState
          -- Remove transactions in block from memPool
          let blockTxs = Block.transactions block
          modifyMemPool_ nodeState $ MP.removeTransactions blockTxs
          -- Update ledger to new ledger state
          setLedger nodeState ledger'
      | otherwise -> logWarning $ ("applyBlock:\n" <>) $
          T.unlines $ map ((<>) "\t" . show) itxs

mineBlock
  :: (MonadIO m, MonadLogger m)
  => NodeState
  -> m (Either NodeStateError Block)
mineBlock nodeState = do
  -- Attempt to mine block
  mPrevBlock <- getLatestBlock nodeState
  case mPrevBlock of
    Nothing -> pure $ Left NoGenesisBlock
    Just prevBlock -> do
      logInfo "mineBlock: Attempting to mine a block..."

      -- Validate and discard invalid transactions
      logInfo "mineBlock: Discarding Invalid Transactions..."
      invalidTxErrs <- purgeMemPool nodeState
      mapM_ logWarning invalidTxErrs
      validTxs <- MP.unMemPool <$> getMemPool nodeState

      -- Attempt to mine block with the valid transactions
      logInfo "mineblock: Constructing new block..."

      let keys = nodeKeys nodeState
      ledger <- getLedger nodeState

      if not (null validTxs)
        then do
          block <- Block.mineBlock prevBlock keys validTxs
          case Block.validateAndApplyBlock ledger prevBlock block of
            Left err -> pure $ Left $ InternalError (show err)
            Right (_, invalidTxErrs')
              | null invalidTxErrs' -> do
                  let blockHashText = decodeUtf8 (Block.hashBlock block)
                  logInfo $ "Generated block with hash:\n\t" <> blockHashText
                  -- Broadcast block message to network
                  let p2pSender = nodeSender nodeState
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
  => NodeState
  -> Address
  -> Int
  -> m Transaction
issueTransfer nodeState toAddr amount = do
  let keys = nodeKeys nodeState
  tx <- liftIO $ Tx.transferTransaction keys toAddr amount
  let p2pSender = nodeSender nodeState
  liftIO . p2pSender $ Msg.TransactionMsg tx
  pure tx

setLedger :: (MonadIO m, MonadLogger m) => NodeState -> Ledger.Ledger -> m ()
setLedger nodeState ledger = do
  logInfo "setLedger: Updating Ledger..."
  modifyNodeState_ nodeState nodeLedger $ \_ -> pure ledger

modifyMemPool_
  :: MonadIO m
  => NodeState
  -> (MemPool -> MemPool)
  -> m ()
modifyMemPool_ nodeState f =
  modifyNodeState_ nodeState nodeMemPool (pure . f)

-- | Removes stale transactions, returning them
purgeMemPool
  :: MonadIO m
  => NodeState
  -> m [Tx.InvalidTx]
purgeMemPool nodeState = do
  ledger <- getLedger nodeState
  txs <- MP.unMemPool <$> getMemPool nodeState
  let (ledger', invalidTxErrs) = Tx.applyTransactions ledger txs
  let invalidTxs = map (\(Tx.InvalidTx tx _) -> tx) invalidTxErrs
  modifyMemPool_ nodeState $ MP.removeTransactions invalidTxs
  return invalidTxErrs

resetMemPool
  :: (MonadIO m, MonadLogger m)
  => NodeState
  -> m ()
resetMemPool nodeState = do
  logInfo "resetMemPool: Resetting memPool..."
  modifyMemPool_ nodeState (const mempty)

-------------------------------------------------------------------------------
-- NodeState Querying
-------------------------------------------------------------------------------

getNodeAddress :: NodeState -> Address
getNodeAddress = Address.deriveAddress . fst . nodeKeys

getBlockChain :: MonadIO m => NodeState -> m Blockchain
getBlockChain = liftIO . readMVar . nodeChain

getLatestBlock :: MonadIO m => NodeState -> m (Maybe Block)
getLatestBlock = fmap head . getBlockChain

getLedger :: MonadIO m => NodeState -> m Ledger.Ledger
getLedger = readMVar' . nodeLedger

getMemPool :: MonadIO m => NodeState -> m MemPool
getMemPool = readMVar' . nodeMemPool

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => MVar a -> m a
readMVar' = liftIO . readMVar

getMVar :: MonadIO m => (a -> MVar b) -> a -> m b
getMVar f = readMVar' . f
