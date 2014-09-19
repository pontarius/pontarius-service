{-# LANGUAGE OverloadedStrings #-}
module Basic where

import Control.Monad.Trans
import DBus
import Data.Text (Text)
import System.Log.Logger

pontariusObjectPath :: ObjectPath
pontariusObjectPath = "/pontarius"

pontariusInterface :: Text
pontariusInterface = "org.pontarius"

logDebug :: MonadIO m => String -> m ()
logDebug = liftIO . debugM "Pontarius.Xmpp.Service"

logError :: MonadIO m => String -> m ()
logError = liftIO . errorM "Pontarius.Xmpp.Service"
