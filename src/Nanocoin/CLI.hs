{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nanocoin.CLI (
  cli
) where

import Protolude
import Prelude (words)

import qualified Data.Map as Map

import Address (Address, mkAddress)
import Nanocoin.Network.Node (NodeState)
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Network.Node as Node

import Options.Applicative

import System.Console.Haskeline hiding (defaultPrefs, catch)

data Query
  = QueryAddress
  | QueryBlocks
  | QueryMemPool
  | QueryLedger
  deriving (Show)

data Cmd
  = CmdMineBlock
  | CmdTransfer Int Address
  deriving (Show)

data CLI
  = Query Query
  | Command Cmd
  deriving (Show)

cli :: NodeState -> IO ()
cli nodeState = runInputT defaultSettings cliLoop
  where
    parserPrefs = defaultPrefs
      { prefShowHelpOnEmpty = True }

    cliLoop = do
      minput <- getInputLine "nanocoin> "
      case minput of
        Nothing -> cliLoop
        Just input -> do
          let cliInputArgs = words input
          mCmdOrQuery <- liftIO $ handleParseResult_ $
            execParserPure parserPrefs (info cliParser mempty) cliInputArgs
          case mCmdOrQuery of
            Nothing -> cliLoop
            Just cmdOrQuery -> do
              liftIO $ handleCLI nodeState cmdOrQuery
              cliLoop

cliParser :: Parser CLI
cliParser = subparser $ mconcat
    [ command "query" $ info (Query <$> queryParser) $
        progDesc "Query the node's state"
    , command "command" $ info (Command <$> cmdParser) $
        progDesc "Issue a command to the node"
    ]
  where
    queryParser = subparser $ mconcat
      [ command "address" $ info (pure QueryAddress) $
          progDesc "Query the node's address"
      , command "blocks"  $ info (pure QueryBlocks) $
          progDesc "Query the node's blocks"
      , command "mempool" $ info (pure QueryMemPool) $
          progDesc "Query the node's transaction pool"
      , command "ledger"  $ info (pure QueryLedger) $
          progDesc "Query the node's ledger"
      ]

    cmdParser = subparser $ mconcat
        [ command "mineblock" $ info (pure CmdMineBlock) $
            progDesc "Mine a block"
        , command "transfer"  $ info transfer $
            progDesc "Tranfer an AMOUNT to an ADDRESS"
        ]
      where
        readAddress :: ReadM Address
        readAddress = eitherReader $ first toS . mkAddress . toS

        transfer = CmdTransfer
          <$> argument auto (metavar "AMOUNT")
          <*> argument readAddress (metavar "ADDRESS")

-- | Handle a cmd line args parser result, discarding the ExitCode
-- XXX Is this ok to do? Just make sure no one shells into the repl.
handleParseResult_ :: ParserResult CLI -> IO (Maybe CLI)
handleParseResult_ pr =
  fmap Just (handleParseResult pr) `catch` \(e :: ExitCode) ->
    pure Nothing

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
          mempool <-  Node.getMemPool nodeState
          let mempool' = MP.unMemPool mempool
          if null mempool'
            then putText "Mempool is empty"
            else myZipWithM_ [1..] mempool' $ \n tx ->
              putText $ show n <> ") " <> show tx

        QueryLedger  -> do
          ledger <- Node.getLedger nodeState
          let ledger' = Map.toList $ L.unLedger ledger
          if null ledger'
            then putText "Ledger is empty"
            else  forM_ ledger' $ \(addr,bal) ->
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
