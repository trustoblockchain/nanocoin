{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nanocoin.TransactionSerializationTest (
  prop_transfer,
  prop_reward,
  tests
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Nanocoin.Transaction as Tran

import qualified Nanocoin.Utils as Utils

prop_transfer :: Property
prop_transfer = property $ do
  pk <- liftIO Utils.publicKey
  transfer <- forAll $ Utils.genTransfer pk
  Utils.encodeThenDecode transfer === Right transfer


prop_reward :: Property
prop_reward = property $ do
  pk <- liftIO Utils.publicKey
  reward <- forAll $ Utils.genReward pk
  Utils.encodeThenDecode reward === Right reward

tests :: IO Bool
tests =
  checkParallel $ Group "Nanocoin.TransactionJsonTest" [
      ("prop_transfer", prop_transfer),
      ("prop_reward", prop_reward)
    ]
