{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nanocoin.CLI (
  cli
) where

import Protolude
import Prelude (words)

import qualified Data.Map as Map

import Options.Applicative

import System.Console.Haskeline hiding (defaultPrefs, catch)

import Logger
import Address (Address, mkAddress)

import Nanocoin.Network.Node (NodeT, NodeEnv, runNodeT)
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.Peer as Peer

data Query
  = QueryAddress
  | QueryBlocks
  | QueryMemPool
  | QueryLedger
  | QueryPeers
  deriving (Show)

data Cmd
  = CmdMineBlock
  | CmdTransfer Int Address
  deriving (Show)

data CLI
  = Query Query
  | Command Cmd
  deriving (Show)

type ConsoleT m a = InputT (NodeT (LoggerT m)) a

cli :: Logger -> NodeEnv -> IO ()
cli logger nodeEnv =
  runLoggerT logger $
    runNodeT nodeEnv $
      runInputT defaultSettings cliLoop
  where
    parserPrefs = defaultPrefs
      { prefShowHelpOnEmpty = True }

    cliLoop :: ConsoleT IO ()
    cliLoop = do
      minput <- getInputLine "nanocoin> "
      case minput of
        Nothing -> cliLoop
        Just input -> do
          let cliInputArgs = words input
          mCmdOrQuery <- liftIO $ handleParseResult_ $
            execParserPure parserPrefs (info cliParser fullDesc) cliInputArgs
          case mCmdOrQuery of
            Nothing -> cliLoop
            Just cmdOrQuery -> do
              lift $ handleCLI cmdOrQuery
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
      , command "peers"  $ info (pure QueryPeers) $
          progDesc "Query the node's known peers"
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

handleCLI
  :: (MonadIO m, MonadLogger m, MonadException m)
  => CLI
  -> NodeT m ()
handleCLI cli =

  case cli of

    Query query ->

      case query of

        QueryAddress -> do
          nodeAddr <- Node.getNodeAddress
          logInfo nodeAddr

        QueryBlocks  -> do
          blocks <- Node.getBlockChain
          mapM_ logInfo blocks

        QueryMemPool -> do
          mempool <-  Node.getMemPool
          let mempool' = MP.unMemPool mempool
          if null mempool'
            then putText "Mempool is empty"
            else myZipWithM_ [1..] mempool' $ \n tx ->
              logInfo $ (show n :: Text) <> ") " <> show tx

        QueryLedger  -> do
          ledger <- Node.getLedger
          let ledger' = Map.toList $ L.unLedger ledger
          if null ledger'
            then putText "Ledger is empty"
            else  forM_ ledger' $ \(addr,bal) ->
              logInfo (mconcat [ show addr," : ", show bal ] :: Text)

        QueryPeers -> do
          peers <- Node.getPeers
          forM_ peers $ \(Peer.Peer pid) ->
            logInfo pid

    Command cmd ->

      case cmd of

        CmdMineBlock        -> do
          eBlock <- Node.mineBlock
          case eBlock of
            Left err    -> logError
              ("Failed to mine block: " <> show err :: Text)
            Right block -> do
              logInfo "Successfully mined block: "
              logInfo block

        CmdTransfer amnt to -> do
          tx <- Node.issueTransfer to amnt
          logInfo "Issued Transfer: "
          logInfo tx

myZipWithM_ xs ys f = zipWithM_ f xs ys
