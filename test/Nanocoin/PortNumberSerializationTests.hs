{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nanocoin.PortNumberSerializationTests (
  prop_portNumberSerialization,
  tests
) where

import Protolude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Network.Socket (PortNumber)

import qualified Nanocoin.Network.Peer as P

import qualified Nanocoin.Utils as Utils

prop_portNumberSerialization :: Property
prop_portNumberSerialization =
  property $ do
    w16 <- forAll $ Gen.word16 (Range.linear 0 (maxBound :: Word16))
    let
      pn = (fromIntegral w16) :: PortNumber
    Utils.encodeThenDecode pn === Right pn


tests :: IO Bool
tests =
  checkParallel $ Group "Nanocoin.TransactionJsonTest" [
      ("prop_portNumberSerialization", prop_portNumberSerialization)
    ]
