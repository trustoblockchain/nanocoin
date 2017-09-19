import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Protolude

import Control.Monad
import System.Exit

import qualified Nanocoin.BlockHeaderSerializationTests as BlockHeaderTests
import qualified Nanocoin.TransactionSerializationTests as TransactionTests
import qualified Nanocoin.PortNumberSerializationTests as PortNumberTests

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
      BlockHeaderTests.tests,
      TransactionTests.tests,
      PortNumberTests.tests
    ]

  Control.Monad.unless (and _results) $
     System.Exit.exitFailure
