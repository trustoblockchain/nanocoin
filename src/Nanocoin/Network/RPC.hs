
module Nanocoin.Network.RPC (
  rpcServer
) where

import Protolude hiding (get, intercalate, print, putText)
import Logger
import qualified System.Logger as Logger

import Data.Aeson hiding (json)
import Data.Text (intercalate)
import Web.Scotty

import Data.List ((\\))
import qualified Data.Map as Map

import Address
import Nanocoin.Network.Node
import Nanocoin.Network.Peer

import qualified Address
import qualified Key
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.Block as B
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Message as Msg


-------------------------------------------------------------------------------
-- RPC (HTTP) Server
-------------------------------------------------------------------------------

-- | Starts an RPC server for interaction via HTTP
rpcServer :: NodeState -> Logger.Logger -> IO ()
rpcServer nodeState logger = do

  let (Peer hostName p2pPort rpcPort) = nodeConfig nodeState
  let p2pSender = nodeSender nodeState

  scotty rpcPort $ do

    defaultHandler $ Logger.err logger . Logger.msg

    --------------------------------------------------
    -- Queries
    --------------------------------------------------

    get "/address" $
      json $ getNodeAddress nodeState

    get "/blocks" $
      queryNodeState nodeState getBlockChain

    get "/mempool" $
      queryNodeState nodeState getMemPool

    get "/ledger" $
      queryNodeState nodeState getLedger

    --------------------------------------------------
    -- Commands
    --------------------------------------------------

    get "/mineBlock" $ do
      eBlock <- runReaderT (mineBlock nodeState) logger
      case eBlock of
        Left err -> text $ show err
        Right block -> json block

    get "/transfer/:toAddr/:amount" $ do
      toAddr' <- param "toAddr"
      amount <- param "amount"
      case mkAddress (encodeUtf8 toAddr') of
        Left err -> text $ toSL err
        Right toAddr -> do
          let keys = nodeKeys nodeState
          tx <- liftIO $ T.transferTransaction keys toAddr amount
          liftIO . p2pSender $ Msg.TransactionMsg tx
          json tx

queryNodeState
  :: ToJSON a
  => NodeState
  -> (NodeState -> IO a)
  -> ActionM ()
queryNodeState nodeState f = json =<< liftIO (f nodeState)
