{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Cmd (
  Cmd(..),
  cmdProc
) where

{-
   This module is necessary because the Process type cannot easily be the base
   monad for some transformer stacks because Control.Distributed.Process does
   not export the `Process` monad datatype. This means we cannot define certain
   MonadX instances for the datatype and, thus, need to build architectural work
   arounds for this problem since these transformer stacks do not support sending
   cloud haskell messages to other processes within them.

   For instance:

   The RPC and CLI modules cannot have `Process` as the base monad in their
   transformer stacks, so instead we have to pass a Control.Concurrent.Chan channel
   to those functions where, whenever a message must be issued to the network from
   these modules, they must feed a `Cmd` value into the channel which is then read
   by the `Cmd` cloud-haskell process, and translated into a network Msg and finally
   relayed to the network.
-}

import Protolude

import Control.Concurrent.Chan (Chan(..), readChan)
import Control.Distributed.Process.Lifted

import Data.Serialize (Serialize)

import Logger
import Nanocoin.Block (Block)
import Nanocoin.Transaction (Transaction)
import Nanocoin.Network.Node (NodeProcessM, nsendAllPeers, getSelfPeer)
import Nanocoin.Network.Message (Msg(..))
import Nanocoin.Network.Service (Service(Messaging))

data Cmd
  = TransactionCmd Transaction
  | BlockCmd Block
  deriving (Eq, Show, Generic, Serialize)

cmdProc :: Chan Cmd -> NodeProcessM ()
cmdProc chan =
  forever $ do
    handleCmd =<<
      liftIO (readChan chan)

handleCmd :: Cmd -> NodeProcessM ()
handleCmd cmd = do
  logInfoText $ "handleCmd: ReceivedCmd: " <> show cmd
  selfPeer <- getSelfPeer
  case cmd of
    TransactionCmd tx ->
      nsendAllPeers Messaging $
        TransactionMsg tx
    BlockCmd block ->
      nsendAllPeers Messaging $
        BlockMsg selfPeer block
