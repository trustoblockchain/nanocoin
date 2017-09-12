{-# LANGUAGE FlexibleInstances #-}
module Logger () where

import System.Logger.Class
import qualified System.Logger as Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

instance MonadIO m => MonadLogger (ReaderT Logger m) where
  log lvl f = do
    logger <- ask
    Logger.log logger lvl f
