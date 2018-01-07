
module Nanocoin.Network.Service (
  Service(..)
) where

import Protolude

-- This module describes the different services (processes) running on a
-- Nanocoin node.
--
-- PeerDiscovery:
--   Handles the discovery of new peers and the removal of dead peers.
--
-- Messaging;
--   Handles basic network messages for communication between nodes.

data Service = PeerDiscovery | Messaging
  deriving (Show)
