{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nanocoin.TransactionSerializationTests (
  prop_transferSerialization,
  prop_rewardSerialization,
  tests
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Nanocoin.Transaction as Tran

import qualified Nanocoin.Utils as Utils

prop_transferSerialization :: Property
prop_transferSerialization = property $ do
  pk <- liftIO Utils.publicKey
  transfer <- forAll $ Utils.genTransfer pk
  Utils.encodeThenDecode transfer === Right transfer


prop_rewardSerialization :: Property
prop_rewardSerialization = property $ do
  pk <- liftIO Utils.publicKey
  reward <- forAll $ Utils.genReward pk
  Utils.encodeThenDecode reward === Right reward

tests :: IO Bool
tests =
  checkParallel $ Group "Nanocoin.TransactionSerializationTests" [
      ("prop_transferSerialization", prop_transferSerialization),
      ("prop_rewardSerialization", prop_rewardSerialization)
    ]
