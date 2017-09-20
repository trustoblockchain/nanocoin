import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Protolude

import Control.Monad
import System.Exit

import Serialization

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
      Serialization.tests
    ]

  Control.Monad.unless (and _results) $
     System.Exit.exitFailure
