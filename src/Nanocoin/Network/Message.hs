{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Message (
  Msg(..),
  msgProc,
) where

import Protolude hiding (msg, put, get)

import Control.Monad.Fail
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import Data.Binary (Binary(..))
import Data.Serialize (Serialize, encode, decode)

import Nanocoin.Block (Block(..))
import Nanocoin.Transaction (Transaction(..), transferTransaction, verifyTxSignature)
import Nanocoin.Network.Peer  (Peer(..))
import Nanocoin.Network.Service
import Nanocoin.Network.Node as Node

import Address
import Logger

data Msg
  = QueryBlockMsg Peer Int
  | BlockMsg Peer Block
  | TransactionMsg Transaction
  deriving (Eq, Show, Generic, Serialize)

instance Binary Msg where
  put = put . encode
  get = do
    eMsg <- decode <$> get
    case eMsg of
      Left err -> fail $ show err
      Right msg -> pure msg

msgProc :: NodeProcessM ()
msgProc =
  controlP $ \runInProc ->
    forever $ receiveWait
      [ match $ runInProc . handleMsg ]

handleMsg :: Msg -> NodeProcessM ()
handleMsg msg = do

  logInfo $ "handleMsg: Received Msg: " <> (show msg :: Text)

  case msg of
    QueryBlockMsg peer n -> do
      chain <- Node.getBlockChain
      case find ((==) n . index) chain of
        Nothing -> logInfoText $ "handleMsg: No block with index " <> show n
        Just block -> do
          selfPeer <- Node.getSelfPeer
          nsendPeer peer Messaging $ BlockMsg selfPeer block

    BlockMsg peer block -> do
      mPrevBlock <- Node.getLatestBlock
      case mPrevBlock of
        Nothing -> logInfo "handleMsg: No Genesis block found."
        Just prevBlock -> do
          -- Apply block to world state
          Node.applyBlock block
          -- If the block was successfully applied
          newPrevBlock <- Node.getLatestBlock
          when (Just block == newPrevBlock) $ do
            selfPeer <- Node.getSelfPeer
            -- Ask if there is a more recent block
            nsendPeer peer Messaging $
              QueryBlockMsg selfPeer (index block + 1)

    TransactionMsg tx -> do
      ledger <- Node.getLedger
      -- Verify Signature before adding to MemPool
      case verifyTxSignature ledger tx of
        Left err -> logError ("handleMsg: " <> show err :: Text)
        Right _  -> Node.addTransactionToMemPool tx
