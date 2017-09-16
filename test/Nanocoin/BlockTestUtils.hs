{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.BlockTestUtils (
  genBlockHeader,
  publicKey
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import qualified Nanocoin.Block as Block
import qualified Key
import qualified Hash

genBlockHeader :: Key.PublicKey -> Gen Block.BlockHeader
genBlockHeader pk =
  Block.BlockHeader
    <$> Gen.constant pk
    <*> genAlphaNumByteString
    <*> genAlphaNumByteString
    <*> genNonce

genAlphaNumByteString :: Gen ByteString
genAlphaNumByteString = toByteString <$> (Gen.string (Range.linear 50 100) Gen.alphaNum)

genNonce :: Gen Int64
genNonce = Gen.int64 (Range.linear 0 (maxBound :: Int64))

publicKey :: IO Key.PublicKey
publicKey = fst <$> Key.newKeyPair

toByteString :: [Char] -> ByteString
toByteString = Hash.getHash . Hash.sha256 . B8.pack
