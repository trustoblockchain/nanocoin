{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.BlockJsonTest where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Key
import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tran

prop_blockJson :: Property
prop_blockJson = Protolude.undefined

genBlock :: Gen Block.Block
genBlock = undefined

genTransfer :: Gen Tran.Transfer
genTransfer = undefined

genReward :: Key.PublicKey -> Gen Tran.Reward
genReward pk =
  Tran.Reward
    <$> genInt
    <*> pk
