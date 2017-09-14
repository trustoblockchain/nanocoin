{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.BlockHeaderJsonTest (
  prop_blockHeader,
  tests
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

import qualified Nanocoin.Block as Block
import qualified Key
import qualified Hash

prop_blockHeader :: Property
prop_blockHeader = property $ do
  pk <- liftIO $ publicKey
  bh <- forAll $ genBlockHeader pk
  json <- return $ encode bh
  let
    (x, y) = Key.extractPoint pk
    mr = Hash.encode64 . Block.merkleRoot $ bh
    prev = Hash.encode64 . Block.previousHash $ bh
    nonce = Block.nonce bh

  json ^? key "origin" . key "x" . _Integer === Just x
  json ^? key "origin" . key "y" . _Integer === Just y
  json ^? key "merkleRoot" . _String  === Just mr
  json ^? key "previousHash" . _String  === Just prev
  (toInt64 <$> json ^? key "nonce" ._Integer) === Just nonce


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

toInt64 :: Integer -> Int64
toInt64 = fromIntegral

tests :: IO Bool
tests = checkSequential $$(discover)
