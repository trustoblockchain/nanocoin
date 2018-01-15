{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Nanocoin.Network.Message (
  Msg(..),

  messagingProc,

) where

import Protolude hiding (msg)

import Control.Monad.Base
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import Data.Serialize (Serialize(..))

import Nanocoin.Block (Block(..))
import Nanocoin.Transaction (Transaction(..), transferTransaction)
import Nanocoin.Network.Service
import Nanocoin.Network.Node

import Address
import Logger

data Msg
  = QueryBlockMsg Int
  | BlockMsg Block
  | TransactionMsg Transaction
  deriving (Eq, Show, Generic, Serialize)

messagingProc :: NodeProcessM ()
messagingProc = undefined

handleMsg :: Msg -> NodeProcessM ()
handleMsg msg = undefined
