{-# LANGUAGE FlexibleInstances #-}
module Logger (
  Level(..),
  Output(..),
  Logger(..),
  create,
  print,
  putText,
  MonadLogger(..)
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

print :: (MonadLogger m, Show a) => a -> m ()
print = info . msg .(show :: Show a => a -> Text)

putText :: MonadLogger m => Text -> m ()
putText = info . msg
