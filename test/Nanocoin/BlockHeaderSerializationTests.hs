{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nanocoin.BlockHeaderSerializationTests (
  prop_blockHeaderSerialization,
  tests
) where

import Protolude

import Hedgehog

import qualified Data.Serialize as S

import qualified Nanocoin.Utils as Utils

prop_blockHeaderSerialization :: Property
prop_blockHeaderSerialization = property $ do
  pk <- liftIO Utils.publicKey
  bh <- forAll $ Utils.genBlockHeader pk
  Utils.encodeThenDecode bh === Right bh

toInt64 :: Integer -> Int64
toInt64 = fromIntegral

tests :: IO Bool
tests =
  checkParallel $ Group "Nanocoin.BlockHeaderSerializationTests" [
      ("prop_blockHeaderSerialization", prop_blockHeaderSerialization)
    ]
