{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

-- XXX RENAME:
-- module Nanocoin.Network.Utils
module Nanocoin.Network.Peer (
  Peer(..),
  Peers,
) where

import Protolude hiding (put, get)

import Data.Aeson (ToJSON(..))
import Data.Binary (Binary, encode, decode)
import Data.Serialize (Serialize(..))

import Control.Distributed.Process (ProcessId, NodeId)
import Control.Distributed.Process.Serializable

import Nanocoin.Network.Utils

type Peers = Set Peer

instance Serialize NodeId where
  put = put . encode
  get = decode <$> get

newtype Peer = Peer { nid :: NodeId }
  deriving (Show, Eq, Ord, Generic, Binary, Typeable, Serializable, Serialize)
