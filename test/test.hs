import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Protolude

import Control.Monad
import System.Exit
import qualified Nanocoin.BlockHeaderJsonTest as BlockHeaderJson
import qualified Nanocoin.TransactionJsonTest as TransactionJson

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
      BlockHeaderJson.tests,
      TransactionJson.tests
    ]

  Control.Monad.unless (and _results) $
     System.Exit.exitFailure
