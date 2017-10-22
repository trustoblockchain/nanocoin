
module Nanocoin.Network.RPC (
  rpcServer
) where

import Protolude hiding (get, intercalate, print, putText)

import Data.Aeson hiding (json, json')
import Data.Text (intercalate)
import Web.Scotty

import Data.List ((\\))
import qualified Data.Map as Map

import Logger
import Address
import qualified Address

import Nanocoin.Network.Node as Node
import Nanocoin.Network.Peer

import qualified Key

import qualified Nanocoin.Ledger as L
import qualified Nanocoin.Block as B
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Message as Msg

-------------------------------------------------------------------------------
-- RPC (HTTP) Server
-------------------------------------------------------------------------------

runNodeT'
  :: NodeState
  -> NodeConfig
  -> NodeT IO a
  -> ActionM a
runNodeT' nodeState nodeConfig = liftIO . runNodeT nodeState nodeConfig

-- | Starts an RPC server for interaction via HTTP
rpcServer :: Logger -> NodeState -> NodeConfig -> IO ()
rpcServer logger nodeState nodeConfig = do

  let runNodeActionM = runNodeT' nodeState nodeConfig

  (Peer hostName p2pPort rpcPort) <-
    liftIO $ runNodeT nodeState nodeConfig $
      gets Node.nodeConfig

  scotty rpcPort $ do

    defaultHandler $ logError' logger . toS

    --------------------------------------------------
    -- Queries
    --------------------------------------------------

    get "/address" $
      json =<< runNodeActionM getNodeAddress

    get "/blocks" $
      json =<< runNodeActionM getBlockChain

    get "/mempool" $
      json =<< runNodeActionM getMemPool

    get "/ledger" $
      json =<< runNodeActionM getLedger

    --------------------------------------------------
    -- Commands
    --------------------------------------------------

    get "/mineBlock" $ do
      eBlock <- runNodeActionM mineBlock
      case eBlock of
        Left err -> text $ show err
        Right block -> json block

    get "/transfer/:toAddr/:amount" $ do
      toAddr' <- param "toAddr"
      amount <- param "amount"
      case mkAddress (encodeUtf8 toAddr') of
        Left err -> text $ toSL err
        Right toAddr -> json =<<
          runNodeActionM (issueTransfer toAddr amount)
