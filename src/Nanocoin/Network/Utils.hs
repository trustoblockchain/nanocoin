
module Nanocoin.Network.Utils (

  HostName,
  P2PPort,
  RPCPort,

  mkNodeId,

) where

import Protolude hiding (put,get,intercalate)

import Control.Distributed.Process.Lifted (NodeId(..))

import Data.Aeson (ToJSON(..))
import Data.ByteString (intercalate)
import Data.Serialize (Serialize, put, get, putWord16be, getWord16be)

import Network.BSD (getHostByName, hostAddress)
import Network.Socket (HostName, PortNumber, inet_ntoa)
import Network.Transport (EndPointAddress(..))

type RPCPort = Int
type P2PPort = PortNumber

instance ToJSON PortNumber where
  toJSON = toJSON . (show :: PortNumber -> Text)

instance Serialize PortNumber where
  put = putWord16be . fromIntegral
  get = fromIntegral <$> getWord16be

mkNodeId :: HostName -> PortNumber -> IO NodeId
mkNodeId hostname' portNum = do
    hostname <- resolveHostname hostname'
    pure $ NodeId $ EndPointAddress $
      intercalate ":" $ [toS hostname, show portNum, "0"]
  where
    resolveHostname =
      inet_ntoa . hostAddress <=< getHostByName
