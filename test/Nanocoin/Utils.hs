{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.Utils (
  genBlockHeader,
  genTransaction,
  genTransfer,
  genReward,
  genAddress,
  publicKey,
  toByteString
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Control.Monad.Trans.Except as Ex

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Key
import qualified Hash
import qualified Address
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tran

type GT = GenT Identity
type Stack = ExceptT Text GT Address.Address

genBlockHeader :: Key.PublicKey -> Gen Block.BlockHeader
genBlockHeader pk =
  Block.BlockHeader
    <$> Gen.constant pk
    <*> genAlphaNumByteString
    <*> genAlphaNumByteString
    <*> genNonce

genTransaction :: Tran.TransactionHeader -> Gen Tran.Transaction
genTransaction th =
  Tran.Transaction
    <$> Gen.constant th
    <*> (toByteString . T.unpack . Hash.encode64 <$> genAlphaNumByteString)

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
