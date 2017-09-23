
module Serialization where

import Protolude

import Data.Serialize as S

import Gen
import Key

import Hedgehog
import Hedgehog.Gen

encodeThenDecode :: Serialize a => a -> Either [Char] a
encodeThenDecode = S.decodeLazy . S.encodeLazy

prop_blockHeaderSerialization :: Property
prop_blockHeaderSerialization = property $ do
  pk <- liftIO $ fst <$> Key.newKeyPair
  bh <- forAll $ Gen.genBlockHeader pk
  encodeThenDecode bh === Right bh

prop_transferSerialization :: Property
prop_transferSerialization = property $ do
  pk <- liftIO $ fst <$> Key.newKeyPair
  transfer <- forAll $ Gen.genTransfer pk
  encodeThenDecode transfer === Right transfer

prop_rewardSerialization :: Property
prop_rewardSerialization = property $ do
  pk <- liftIO $ fst <$> Key.newKeyPair
  reward <- forAll $ Gen.genReward pk
  encodeThenDecode reward === Right reward

tests :: IO Bool
tests = checkParallel $
  Group "Nanocoin.SerializationTests"
    [ ("prop_blockHeaderSerialization", prop_blockHeaderSerialization)
    , ("prop_transferSerialization", prop_transferSerialization)
    , ("prop_rewardSerialization", prop_rewardSerialization)
    ]
