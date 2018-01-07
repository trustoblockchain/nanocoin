{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Message (
  Msg(..),
) where

import Protolude hiding (msg)

import qualified Data.Serialize as S

import Nanocoin.Block
import Nanocoin.Transaction
import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Peer as Peer

data Msg
  = QueryBlockMsg Int
  | BlockMsg Block
  | TransactionMsg Transaction
  deriving (Eq, Show, Generic, S.Serialize)
