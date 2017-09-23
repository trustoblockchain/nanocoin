{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen (
  genBlockHeader,
  genTransfer,
  genReward,
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString.Char8 as B8

import qualified Key
import qualified Hash
import qualified Address
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tran

----------------------------------------------------------------
-- Block Gen
----------------------------------------------------------------

genBlockHeader :: Key.PublicKey -> Gen Block.BlockHeader
genBlockHeader pk =
  Block.BlockHeader
    <$> Gen.constant pk
    <*> genAlphaNumByteString
    <*> genAlphaNumByteString
    <*> genNonce

genNonce :: Gen Int64
genNonce = Gen.int64 (Range.constant 0 (maxBound :: Int64))

----------------------------------------------------------------
-- Transaction Gen
----------------------------------------------------------------

genTransfer :: Key.PublicKey -> Gen Tran.Transfer
genTransfer pk =
  Tran.Transfer
    <$> Gen.constant pk
    <*> genAddress pk
    <*> genInt

genReward :: Key.PublicKey -> Gen Tran.Reward
genReward pk =
  Tran.Reward
    <$> Gen.constant pk
    <*> genInt

----------------------------------------------------------------
-- Address Gen
----------------------------------------------------------------

genAddress :: Key.PublicKey -> Gen Address.Address
genAddress = Gen.constant . Address.deriveAddress

----------------------------------------------------------------
-- Helpers Gen
----------------------------------------------------------------

genAlphaNumByteString :: Gen ByteString
genAlphaNumByteString = B8.pack <$>
  Gen.string (Range.constant 50 100) Gen.alphaNum

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 (maxBound :: Int))
