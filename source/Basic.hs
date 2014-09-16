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

debug :: MonadIO m => String -> m ()
debug = liftIO . debugM "Pontarius.Xmpp"
