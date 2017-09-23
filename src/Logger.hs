{-# LANGUAGE FlexibleInstances #-}
module Logger (
  Logger(..),
  MonadLogger(..),

  mkLogger,
  runLogger,

  logInfo,
  logWarning,
  logError,
  logError',
) where

import Protolude hiding (print, putText, ask)
import System.Logger.Class
import qualified System.Logger as Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

instance MonadIO m => MonadLogger (ReaderT Logger m) where
  log lvl f = do
    logger <- ask
    Logger.log logger lvl f

runLogger = flip runReaderT

mkLogger :: MonadIO m => Maybe FilePath -> m Logger
mkLogger mfp =
  case mfp of
    Nothing -> create Logger.StdOut
    Just fp -> create $ Logger.Path fp

logInfo :: (MonadLogger m, Show a) => a -> m ()
logInfo = info . msg . (show :: Show a => a -> Text)

logWarning :: (MonadLogger m, Show a) => a -> m ()
logWarning = warn . msg . (show :: Show a => a -> Text)

logError :: (MonadLogger m, Show a) => a -> m ()
logError = err . msg . (show :: Show a => a -> Text)

logError' :: MonadIO m => Logger -> Text -> m ()
logError' logger = flip runReaderT logger . logError
