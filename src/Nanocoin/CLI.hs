{-# LANGUAGE LambdaCase #-}

module Nanocoin.CLI (
  cli
) where

import Protolude
import Prelude (words)

import qualified Data.Map as Map

import Address (Address)
import Nanocoin.Network.Node (NodeState)
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Network.Node as Node

import Options.Applicative

import System.Console.Haskeline hiding (defaultPrefs)

data Query
  = QueryAddress
  | QueryBlocks
  | QueryMemPool
  | QueryLedger

data Cmd
  = CmdMineBlock
  | CmdTransfer Int Address

data CLI
  = Query Query
  | Command Cmd

cli :: NodeState -> InputT IO ()
cli nodeState = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "nanocoin> "
      case minput of
        Nothing -> pure ()
        Just input -> do
          let cliInputArgs = words input
          cmdOrQuery <- liftIO $ handleParseResult $
            execParserPure defaultPrefs (info cliParser mempty) cliInputArgs
          liftIO $ handleCLI nodeState cmdOrQuery

cliParser :: Parser CLI
cliParser = notImplemented -- XXX

handleCLI :: NodeState -> CLI -> IO ()
handleCLI nodeState cli =

  case cli of

    Query query ->
      case query of
        QueryAddress -> do
          let nodeAddr = Node.getNodeAddress nodeState
          putText $ show nodeAddr
        QueryBlocks  -> do
          blocks <- Node.getBlockChain nodeState
          mapM_ print blocks
        QueryMemPool -> do
          mempool <- Node.getMemPool nodeState
          myZipWithM_ [1..] (MP.unMemPool mempool) $ \n tx ->
            putText $ show n <> ") " <> show tx
        QueryLedger  -> do
          ledger <- Node.getLedger nodeState
          forM_ (Map.toList $ L.unLedger ledger) $ \(addr,bal) ->
            putText $ show addr <> " : " <> show bal

    Command cmd ->
      case cmd of
        CmdMineBlock        -> do
          eBlock <- Node.mineBlock nodeState
          case eBlock of
            Left err    -> putText $ "Error mining block: " <> show err
            Right block -> putText "Successfully mined block: " >> print block
        CmdTransfer amnt to -> do
          tx <- Node.issueTransfer nodeState to amnt
          putText "Issued Transfer: " >> print tx

myZipWithM_ xs ys f = zipWithM_ f xs ys
