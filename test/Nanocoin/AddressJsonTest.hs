{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nanocoin.AddressJsonTest where

import Protolude

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T

import qualified Address

import qualified Nanocoin.Utils as Utils

prop_addressJson :: Property
prop_addressJson = property $ do
  pk <- liftIO Utils.publicKey
  addr <- forAll $ Utils.genAddress pk
  json <- return $ encode addr
  let
    rawAddress = Just $ decodeUtf8 $ Address.rawAddress addr

  json ^? _String === rawAddress
  (decode json :: Maybe Address.Address) === Just addr

tests :: IO Bool
tests = checkSequential $$(discover)
