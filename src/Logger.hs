{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Logger (

  Logger,
  MonadLogger(..),

  LoggerT,
  runLoggerT,

  mkLogger,

  logInfo,
  logWarning,
  logError,
) where

import Protolude hiding (log)

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import System.Console.Haskeline.MonadException (MonadException)
import System.Logger.Class
import qualified System.Logger as Logger

newtype LoggerT m a = LoggerT { unLoggerT :: ReaderT Logger m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Logger, MonadException)

--------------------------------------------------------------------------------

instance MonadIO m => MonadLogger (LoggerT m) where
  log lvl f = do
    logger <- ask
    liftIO $ Logger.log logger lvl f

deriving instance MonadBase IO m => MonadBase IO (LoggerT m)

instance MonadTransControl LoggerT where
  type StT LoggerT a = StT (ReaderT Logger) a
  liftWith = defaultLiftWith LoggerT unLoggerT
  restoreT = defaultRestoreT LoggerT

instance MonadBaseControl IO m => MonadBaseControl IO (LoggerT m) where
  type StM (LoggerT m) a = ComposeSt LoggerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadProcess m => MonadProcess (LoggerT m) where
  liftP = LoggerT . liftP

instance MonadProcessBase m => MonadProcessBase (LoggerT m) where
  type StMP (LoggerT m) a = ComposeStP LoggerT m a
  liftBaseWithP = defaultLiftBaseWithP
  restoreMP = defaultRestoreMP

--------------------------------------------------------------------------------

runLoggerT :: Logger -> LoggerT m a -> m a
runLoggerT logger = flip runReaderT logger . unLoggerT

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
