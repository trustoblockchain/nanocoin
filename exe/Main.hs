module Main where

import Protolude hiding (option)

import Data.Maybe (fromMaybe)

import Nanocoin (initNode)
import Nanocoin.Network.Utils (RPCPort, P2PPort)

import Options.Applicative
import Logger

data Config = Config
  { rpcPort      :: RPCPort
  , p2pPort      :: P2PPort
  , keysPath     :: Maybe FilePath
  , logFilepath  :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig = Config
  { rpcPort     = 3000
  , p2pPort     = fromIntegral (8001 :: Int)
  , keysPath    = Nothing
  , logFilepath = Nothing
  }

main :: IO ()
main = do
    Config rpc p2p mKeys mLogFile <- execParser (info parser mempty)
    logger <- mkLogger mLogFile
    initNode rpc p2p mKeys logger
  where
    intToP2PPort :: Int -> P2PPort
    intToP2PPort = fromIntegral

    rpcPortParser :: Parser (Maybe RPCPort)
    rpcPortParser = optional $
      option auto $ long "rpc-port"
                 <> short 'p'
                 <> metavar "RPC_PORT"

    p2pPortParser :: Parser (Maybe P2PPort)
    p2pPortParser =
      fmap (fmap intToP2PPort) $
        optional $ option auto $
             long "p2p-port"
          <> short 'n'
          <> metavar "P2P_PORT"

    keysParser :: Parser (Maybe FilePath)
    keysParser = optional $
      strOption $ long "keys"
               <> short 'k'
               <> metavar "KEYS_DIR"

    logFileParser :: Parser (Maybe FilePath)
    logFileParser = optional $
      strOption $ long "logfile"
              <> short 'f'
              <> metavar "LOG_FILE"

    parser = Config
      <$> (fromMaybe (rpcPort defaultConfig) <$> rpcPortParser)
      <*> (fromMaybe (p2pPort defaultConfig) <$> p2pPortParser)
      <*> keysParser
      <*> logFileParser
