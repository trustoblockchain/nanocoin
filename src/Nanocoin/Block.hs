{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin.Block (
  Block(..),
  BlockHeader(..),
  Blockchain,
  genesisBlock,

  -- ** Block Hashing
  hashBlock,
  hashBlockHeader,

  -- ** Validation
  InvalidBlock(..),
  validateBlock,
  applyBlock,
  validateAndApplyBlock,

  -- ** Consensus
  proofOfWork,
  validateProofOfWork,
  mineBlock,
  getLatestBlock,


) where

import Protolude

import Control.Monad (fail)

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Text.Encoding as T

import Crypto.Hash.MerkleTree

import Address
import Nanocoin.Ledger
import Nanocoin.Transaction (Transaction)

import qualified Hash
import qualified Key
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Ledger as Ledger

type Index      = Int
type Timestamp  = Integer
type Blockchain = [Block]

data BlockHeader = BlockHeader
  { origin       :: Key.PublicKey -- ^ Address of Block miner
  , previousHash :: ByteString    -- ^ Previous block hash
  , merkleRoot   :: ByteString    -- ^ Merkle Root of transactions
  , nonce        :: Int64         -- ^ Nonce for Proof-of-Work
  } deriving (Eq, Show)

data Block = Block
  { index        :: Index         -- ^ Block height
  , header       :: BlockHeader   -- ^ Block header
  , transactions :: [Transaction] -- ^ List of Transactions
  , signature    :: ByteString    -- ^ Block signature
  } deriving (Eq, Show, Generic, S.Serialize)

genesisBlock :: Key.KeyPair -> IO Block
genesisBlock (pubKey, privKey) = do
    signature' <- liftIO $
      Key.sign privKey (S.encode genesisBlockHeader)
    return Block
      { index     = 0
      , header    = genesisBlockHeader
      , transactions = []
      , signature = S.encode signature'
      }
  where
    genesisBlockHeader = BlockHeader
      { origin       = pubKey
      , previousHash = "0"
      , merkleRoot   = getMerkleRoot emptyHash
      , nonce        = 0
      }

-- | Get the latest block from the chain
getLatestBlock :: Blockchain -> Maybe Block
getLatestBlock = head

-------------------------------------------------------------------------------
-- Block Hashing
-------------------------------------------------------------------------------

-- | Hash a block header, to be used as the prevHash field in Block
hashBlockHeader :: BlockHeader -> ByteString
hashBlockHeader BlockHeader{..} =
  Hash.getHash $ Hash.sha256 $
    BS.concat [ rawAddress (deriveAddress origin)
              , previousHash
              , merkleRoot
              , B8.pack (show nonce)
              ]

-- | Generate a block hash
hashBlock :: Block -> ByteString
hashBlock = hashBlockHeader . header

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

data InvalidBlock
  = InvalidBlockSignature Text
  | InvalidBlockIndex Int
  | InvalidBlockHash
  | InvalidBlockMerkleRoot Text
  | InvalidBlockNumTxs
  | InvalidBlockNoReward
  | InvalidBlockTx T.InvalidTx
  | InvalidRewardTx Int Int
  | InvalidPrevBlockHash
  | InvalidFirstBlock
  | InvalidBlockTxs [T.InvalidTx]
  deriving (Show, Eq)

-- | Verify a block's ECDSA signature
verifyBlockSignature
  :: Block
  -> Either InvalidBlock ()
verifyBlockSignature b = do
  let originKey = origin $ header b
  case S.decode (signature b) of
    Left err -> Left $ InvalidBlockSignature (toS err)
    Right sig -> do
      let validSig = Key.verify originKey sig (S.encode $ header b)
      unless validSig $
        Left $ InvalidBlockSignature "Could not verify block signature."

-- | Validate a block before accepting a block as new block in chain
validateBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock ()
validateBlock ledger prevBlock block
  | index block /= index prevBlock + 1 = Left $ InvalidBlockIndex (index block)
  | hashBlock prevBlock /= previousHash (header block) = Left InvalidPrevBlockHash
  | not (validateProofOfWork block) = Left InvalidBlockHash
  | null (transactions block) = Left InvalidBlockNumTxs
  | mRoot /= mRoot' = Left $ InvalidBlockMerkleRoot $ toS mRoot'
  | otherwise = do
      -- Validate Reward Transaction
      validateBlockReward block
      -- Verify signature of block
      verifyBlockSignature block
      -- Validate all transactions w/ respect to world state
      first InvalidBlockTx $
        T.validateTransactions ledger blockTxs
  where
    blockTxs = transactions block
    txHashes = map T.hashTransaction blockTxs
    mRoot  = merkleRoot $ header block      -- given root
    mRoot' = mtHash $ mkMerkleTree txHashes -- constr root

-- | Validates a block with a reward transaction at the end
-- 1) There must be only 1 reward transaction
-- 2) Reward TX must be the last transaction
-- 3) The amount in the reward transaction must be dependent on block idx
-- 4) The public key of the Reward TX must match the block origin
validateBlockReward
  :: Block
  -> Either InvalidBlock ()
validateBlockReward block = go $ transactions block
  where
    -- Walk the list of transactions, assuring only 1
    -- Reward transaction and enforcing the other predicates
    go [] = Left InvalidBlockNumTxs
    go [tx] =
      case T.header tx of
        T.RewardHeader (T.Reward mk amnt) -> do
          let rewardAmnt = calcReward (index block)
          let blockOrigin = origin $ header block
          unless (rewardAmnt == amnt && mk == blockOrigin) $
            Left $ InvalidRewardTx rewardAmnt amnt
        otherwise -> Left InvalidBlockNoReward
    go (tx1:tx2:txs) =
      case T.header tx1 of
        T.RewardHeader (T.Reward mk amnt) ->
          Left $ InvalidRewardTx amnt amnt
        otherwise -> go (tx2:txs)

validateAndApplyBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock (Ledger, [T.InvalidTx])
validateAndApplyBlock ledger prevBlock block = do
  validateBlock ledger prevBlock block
  Right $ applyBlock ledger block

-- | Apply block transactions to world state
applyBlock
  :: Ledger
  -> Block
  -> (Ledger, [T.InvalidTx])
applyBlock ledger = T.applyTransactions ledger . transactions

-------------------------------------------------------------------------------
-- Consensus
-------------------------------------------------------------------------------

-- | Generates (mines) a new block using the `proofOfWork` function
mineBlock
  :: MonadIO m
  => Block          -- ^ Previous Block in chain
  -> Key.KeyPair    -- ^ Miner's ECDSA key pair
  -> [Transaction]  -- ^ List of transactions
  -> m Block
mineBlock prevBlock keys@(pubKey,privKey) txs' = do
    -- Generate reward transaction
    rewardTx <- liftIO $
      T.rewardTransaction keys (calcReward index')

    -- Create the block header
    let blockTxs = txs' ++ [rewardTx]
    let blockHeader = mkBlockHeader blockTxs

    -- Sign the serialized block header
    signature' <- liftIO $
      Key.sign privKey (S.encode blockHeader)
    return Block
      { index        = index'
      , header       = blockHeader
      , transactions = blockTxs
      , signature    = S.encode signature'
      }
  where
    index'      = index prevBlock + 1
    prevHash    = hashBlock prevBlock
    origin'     = Key.toPublic privKey

    mkBlockHeader txs = proofOfWork index' $
      let txHashes = map T.hashTransaction txs
      in BlockHeader { origin       = origin'
                     , previousHash = prevHash
                     , merkleRoot   = mtHash (mkMerkleTree txHashes)
                     , nonce        = 0
                     }

    now :: IO Integer
    now = round `fmap` getPOSIXTime

proofOfWork
  :: Int         -- ^ Difficulty measured by block index
  -> BlockHeader -- ^ Header to hash with nonce parameter
  -> BlockHeader
proofOfWork idx blockHeader = blockHeader { nonce = calcNonce 0 }
  where
    difficulty = calcDifficulty idx
    prefix = toS $ replicate difficulty '0'

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        headerHash = hashBlockHeader (blockHeader { nonce = n })
        prefix' = BS.take difficulty headerHash

-- | difficulty(block) = round(ln(index(block)))
calcDifficulty :: Int -> Int
calcDifficulty = round . logBase (2 :: Float) . fromIntegral

validateProofOfWork :: Block -> Bool
validateProofOfWork block =
    BS.isPrefixOf prefix $ hashBlock block
  where
    difficulty = calcDifficulty $ index block
    prefix = toS $ replicate difficulty '0'

-- | Calculate the reward (difficulty x 100)
calcReward :: Int -> Int
calcReward = (*) 100 . calcDifficulty

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance S.Serialize BlockHeader where
  put (BlockHeader opk ph mr n) = do
    Key.putPublicKey opk
    S.put ph
    S.put mr
    S.put n
  get = BlockHeader
    <$> Key.getPublicKey
    <*> S.get
    <*> S.get
    <*> S.get

instance ToJSON BlockHeader where
  toJSON (BlockHeader opk ph mr n) =
    let (x,y) = Key.extractPoint opk in
    object [ "origin"       .= object
               [ "x" .= (x :: Integer)
               , "y" .= (y :: Integer)
               ]
           , "previousHash" .= Hash.encode64 ph
           , "merkleRoot"   .= Hash.encode64 mr
           , "nonce"        .= toJSON n
           ]

instance ToJSON Block where
  toJSON (Block i bh txs s) =
    object [ "index"        .= i
           , "header"       .= toJSON bh
           , "transactions" .= toJSON txs
           , "signature"    .= Hash.encode64 s
           ]
