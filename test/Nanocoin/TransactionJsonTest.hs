{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.TransactionJsonTest where

import Protolude

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import qualified Key
import qualified Address
import qualified Hash
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tran

import qualified Nanocoin.Utils as Utils

prop_transfer :: Property
prop_transfer = property $ do
  pk <- liftIO Utils.publicKey
  transfer <- forAll $ Utils.genTransfer pk
  json <- return $ encode transfer

  assertTransfer (Just json) transfer


prop_reward :: Property
prop_reward = property $ do
  pk <- liftIO Utils.publicKey
  reward <- forAll $ Utils.genReward pk
  json <- return $ encode reward

  assertReward (Just json) reward

prop_transaction :: Property
prop_transaction = property $ do
  pk <- liftIO Utils.publicKey
  transHeader <- forAll $ genTransactionHeader pk
  transaction <- forAll $ Utils.genTransaction transHeader
  json <- return $ encode transaction
  let
    signature = Hash.encode64 $ Tran.signature transaction
    actualTag = (json ^? key "header" . key "tag" . _String)

  json ^? key "signature" . _String === Just signature
  assertTransactionHeader (json ^? _Value) transHeader

genTransactionHeader :: Key.PublicKey -> Gen Tran.TransactionHeader
genTransactionHeader pk =
  Gen.recursive Gen.choice [
    Tran.TransferHeader <$> Utils.genTransfer pk
  ] [
    Tran.RewardHeader <$> Utils.genReward pk
  ]

assertTransactionHeader :: (AsValue t, MonadTest m) => Maybe t -> Tran.TransactionHeader -> m ()
assertTransactionHeader (Just json) h =
  let
    tagJson = json ^? key "header" . key "tag" . _String
    headerJson = json ^? key "header" . key "contents"
  in
    case h of
      Tran.TransferHeader h -> tagJson === Just "TransferHeader" >> assertTransfer headerJson h
      Tran.RewardHeader h   -> tagJson === Just "RewardHeader" >> assertReward headerJson h

assertTransfer :: (AsValue t, MonadTest m) => Maybe t -> Tran.Transfer -> m ()
assertTransfer (Just json) transfer =
  do
    let
      (x, y)    = Key.extractPoint . Tran.senderKey $ transfer
      addr      = Tran.recipient transfer
      recipient = Address.rawAddress addr
      amount    = Tran.amount transfer

    (json ^? key "senderKey" . key "x" . _Integer)       === Just x
    (json ^? key "senderKey" . key "y" . _Integer)       === Just y
    (encodeUtf8 <$> (json ^? key "recipient" . _String)) === Just recipient
    (toInt <$> json ^? key "amount" . _Integer)          === Just amount

assertReward :: (AsValue t, MonadTest m) => Maybe t -> Tran.Reward -> m ()
assertReward (Just json) reward =
  do
    let
      (x, y) = Key.extractPoint . Tran.minerKey $ reward
      reward' = Tran.reward reward

    json ^? key "minerKey" . key "x" . _Integer === Just x
    json ^? key "minerKey" . key "y" . _Integer === Just y
    (toInt <$> json ^? key "reward" . _Integer) === Just reward'

toInt :: Integer -> Int
toInt = fromIntegral

tests :: IO Bool
tests = checkSequential $$(discover)
