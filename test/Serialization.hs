{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialization (
  prop_blockHeaderSerialization,
  prop_portNumberSerialization,
  prop_transferSerialization,
  prop_rewardSerialization,
  tests
) where

import Protolude

import Hedgehog
import Hedgehog.Gen

import Network.Socket (PortNumber)

import qualified Nanocoin.Network.Peer as P

import Gen

prop_blockHeaderSerialization :: Property
prop_blockHeaderSerialization = property $ do
  pk <- liftIO Gen.publicKey
  bh <- forAll $ Gen.genBlockHeader pk
  Gen.encodeThenDecode bh === Right bh

prop_portNumberSerialization :: Property
prop_portNumberSerialization =
  property $ do
    w16 <- forAll $ genWord16
    let
      pn = (fromIntegral w16) :: PortNumber
    Gen.encodeThenDecode pn === Right pn

prop_transferSerialization :: Property
prop_transferSerialization = property $ do
  pk <- liftIO Gen.publicKey
  transfer <- forAll $ Gen.genTransfer pk
  Gen.encodeThenDecode transfer === Right transfer

prop_rewardSerialization :: Property
prop_rewardSerialization = property $ do
  pk <- liftIO Gen.publicKey
  reward <- forAll $ Gen.genReward pk
  Gen.encodeThenDecode reward === Right reward

tests :: IO Bool
tests =
  checkParallel $ Group "Nanocoin.SerializationTests" [
      ("prop_blockHeaderSerialization", prop_blockHeaderSerialization),
      ("prop_portNumberSerialization", prop_portNumberSerialization),
      ("prop_transferSerialization", prop_transferSerialization),
      ("prop_rewardSerialization", prop_rewardSerialization)
    ]
