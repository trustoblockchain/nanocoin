{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.BlockHeaderJsonTest (
  prop_blockHeaderJson,
  tests
) where

import Protolude

import Hedgehog

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

import qualified Nanocoin.Block as Block
import qualified Key
import qualified Hash
import qualified Nanocoin.BlockTestUtils as Utils

prop_blockHeaderJson :: Property
prop_blockHeaderJson = property $ do
  pk <- liftIO $ Utils.publicKey
  bh <- forAll $ Utils.genBlockHeader pk
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

toInt64 :: Integer -> Int64
toInt64 = fromIntegral

tests :: IO Bool
tests = checkSequential $$(discover)
