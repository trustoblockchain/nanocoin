{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Nanocoin.Utils (
  genBlockHeader,
  genTransfer,
  genReward,
  publicKey,
  encodeThenDecode
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Serialize as S
import qualified Data.ByteString.Char8 as B8

import qualified Key
import qualified Hash
import qualified Address
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tran

genBlockHeader :: Key.PublicKey -> Gen Block.BlockHeader
genBlockHeader pk =
  Block.BlockHeader
    <$> Gen.constant pk
    <*> genAlphaNumByteString
    <*> genAlphaNumByteString
    <*> genNonce

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

encodeThenDecode :: (S.Serialize a) => a -> Either String a
encodeThenDecode = S.decodeLazy . S.encodeLazy

genAddress :: Key.PublicKey -> Gen Address.Address
genAddress = Gen.constant . Address.deriveAddress

genAlphaNumByteString :: Gen ByteString
genAlphaNumByteString = toByteString <$> (Gen.string (Range.constant 50 100) Gen.alphaNum)

genNonce :: Gen Int64
genNonce = Gen.int64 (Range.constant 0 (maxBound :: Int64))

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 (maxBound :: Int))

publicKey :: IO Key.PublicKey
publicKey = fst <$> Key.newKeyPair

toByteString :: [Char] -> ByteString
toByteString = Hash.getHash . Hash.sha256 . B8.pack
