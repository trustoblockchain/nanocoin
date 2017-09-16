{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.BlockJsonTest where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Nanocoin.Block as Block
import qualified Nanocoin.Transaction as Tran

prop_blockJson :: Property
prop_blockJson = Protolude.undefined

genBlock :: Gen Block.Block
genBlock = undefined

genIndex :: Gen Int
genIndex = Gen.int (Range.linear 0 (maxBound :: Int))

genTransaction :: Gen Tran.Transaction
genTransaction = undefined

genTransactionHeader :: Gen Tran.TransactionHeader
genTransactionHeader = undefined

genTransfer :: Gen Tran.Transfer
genTransfer = undefined

genReward :: Gen Tran.Reward
genReward = undefined
