import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Protolude

import Control.Monad
import System.Exit

import qualified Nanocoin.BlockHeaderSerializationTest as BlockHeaderTests
import qualified Nanocoin.TransactionSerializationTest as TransactionTests

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
      BlockHeaderTests.tests,
      TransactionTests.tests
    ]

  Control.Monad.unless (and _results) $
     System.Exit.exitFailure
