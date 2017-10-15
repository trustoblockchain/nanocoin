

module Time (
  Timestamp(..),
  now
) where

import Protolude

import Data.Time.Clock.POSIX (getPOSIXTime)

type Timestamp = Int64

-- | Time now (in microseconds)
now :: IO Timestamp
now = round <$> (* 1000000) <$> getPOSIXTime
