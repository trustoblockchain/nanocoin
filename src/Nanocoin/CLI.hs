{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nanocoin.CLI (
  cli
) where

import Protolude
import Prelude (words)

import qualified Data.Map as Map

import Logger
import Address (Address, mkAddress)

import Nanocoin.Network.Message (Msg)
import Nanocoin.Network.Node (NodeT, NodeEnv, runNodeT)
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.MemPool as MP
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Cmd as Cmd
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Peer as Peer

import Options.Applicative
import System.Console.Haskeline hiding (defaultPrefs, catch)

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

data Arg
  = Query Query
  | Command Cmd
  deriving (Show)

type ConsoleT m a = InputT (NodeT (LoggerT m)) a

cli :: Logger -> NodeEnv -> Chan Cmd.Cmd -> IO ()
cli logger nodeEnv cmdChan =
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
              lift $ handleArg cmdChan cmdOrQuery
              cliLoop

cliParser :: Parser Arg
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
handleParseResult_ :: ParserResult Arg -> IO (Maybe Arg)
handleParseResult_ pr =
  fmap Just (handleParseResult pr) `catch` \(e :: ExitCode) ->
    pure Nothing

handleArg
  :: (MonadLogger m, MonadException m)
  => Chan Cmd.Cmd
  -> Arg
  -> NodeT m ()
handleArg cmdChan cli =

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
              logInfoMsg "Broadcasting block to the network:" block
              let blockCmd = Cmd.BlockCmd block
              liftIO $ writeChan cmdChan blockCmd

        CmdTransfer amnt to -> do
          keys <- Node.getNodeKeys
          tx <- liftIO $ T.transferTransaction keys to amnt
          let txCmd = Cmd.TransactionCmd tx
          logInfoMsg "Broadcasting transaction to network:" tx
          liftIO $ writeChan cmdChan txCmd

myZipWithM_ xs ys f = zipWithM_ f xs ys
