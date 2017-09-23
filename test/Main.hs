
module Main where

import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Protolude

import Control.Monad
import System.Exit

import Serialization

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [ Serialization.tests ]

  unless (and results) $ exitFailure
