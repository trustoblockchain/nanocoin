
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

import Nanocoin.Network.Message (Msg(..))
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

runNodeActionM
  :: Logger
  -> NodeEnv
  -> NodeT (LoggerT IO) a
  -> ActionM a
runNodeActionM logger nodeEnv =
  liftIO . runLoggerT logger . runNodeT nodeEnv

-- | Starts an RPC server for interaction via HTTP
rpcServer
  :: Logger
  -> NodeEnv
  -> Chan Msg
  -> IO ()
rpcServer logger nodeEnv msgChan = do

  (NodeConfig hostName p2pPort rpcPort keys) <-
    liftIO $ nodeConfig <$> runNodeT nodeEnv ask

  let runNodeActionM' = runNodeActionM logger nodeEnv

  scotty rpcPort $ do

    --------------------------------------------------
    -- Queries
    --------------------------------------------------

    get "/address" $
      json =<< runNodeActionM' getNodeAddress

    get "/blocks" $
      json =<< runNodeActionM' getBlockChain

    get "/mempool" $
      json =<< runNodeActionM' getMemPool

    get "/ledger" $
      json =<< runNodeActionM' getLedger

    --------------------------------------------------
    -- Commands
    --------------------------------------------------

    get "/mineBlock" $ do
      eBlock <- runNodeActionM' mineBlock
      case eBlock of
        Left err -> text $ show err
        Right block -> do
          let blockMsg = Msg.BlockMsg block
          liftIO $ writeChan msgChan blockMsg
          json block

    get "/transfer/:toAddr/:amount" $ do
      toAddr' <- param "toAddr"
      amount <- param "amount"
      case mkAddress (encodeUtf8 toAddr') of
        Left err -> text $ toSL err
        Right toAddr -> do
          keys <- runNodeActionM' Node.getNodeKeys
          tx <- liftIO $ T.transferTransaction keys toAddr amount
          let txMsg = Msg.TransactionMsg tx
          liftIO $ writeChan msgChan txMsg
          json tx
